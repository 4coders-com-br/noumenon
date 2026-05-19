(ns noumenon.llm
  "LLM invocation against any Anthropic-Messages-API-compatible endpoint.

   Configuration collapses to three env vars:
     NOUMENON_LLM_BASE_URL  (required)  endpoint, e.g. https://api.anthropic.com
     NOUMENON_LLM_API_KEY   (required)  bearer / x-api-key value
     NOUMENON_LLM_MODEL     (optional)  default model id, used when caller omits :model

   Credentials resolve from env first, then from `~/.noumenon/credentials`
   as a fallback. The file fallback is gated by the JVM system property
   `noumenon.allow-file-credentials` (default \"true\"). The HTTP daemon sets
   it to \"false\" at startup when bound to anything other than 127.0.0.1, so
   shared-service deployments cannot leak a user's on-disk credentials."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [noumenon.util :as util :refer [log! truncate]]
            [org.httpkit.client :as http])
  (:import [java.net URI]))

;; --- Pricing ---

(def model-pricing
  "Per-token pricing in $/1M tokens for direct Anthropic API models.
   Keys are matched as prefixes against the model id returned by the
   provider, so both bare names (claude-sonnet-4-6) and date-stamped
   ids (claude-sonnet-4-6-20250514) hit the same entry. Unknown models
   (anything not prefix-matched) return 0."
  {"claude-sonnet-4-6" {:input 3.0  :output 15.0}
   "claude-haiku-4-5"  {:input 0.80 :output 4.0}
   "claude-opus-4-6"   {:input 15.0 :output 75.0}
   "claude-opus-4-7"   {:input 15.0 :output 75.0}})

(defn estimate-cost
  "Estimate USD cost for given model and token counts. Returns 0.0 for unknown models."
  [model-id input-tokens output-tokens]
  (if-let [{:keys [input output]} (some (fn [[k v]]
                                          (when (and model-id (str/starts-with? model-id k)) v))
                                        model-pricing)]
    (+ (* input-tokens (/ input 1e6))
       (* output-tokens (/ output 1e6)))
    0.0))

;; --- Usage tracking ---

(def zero-usage
  "Default usage map with all fields zeroed."
  {:input-tokens 0 :output-tokens 0 :cost-usd 0.0 :duration-ms 0})

(defn sum-usage
  "Sum two usage maps. Treats nil as zero-usage."
  [a b]
  (merge-with + (or a zero-usage) (or b zero-usage)))

;; --- Config resolution ---

(def ^:private file-fallback-property "noumenon.allow-file-credentials")

(defn- file-fallback-enabled?
  "True unless the JVM was explicitly told to skip ~/.noumenon/credentials."
  []
  (not= "false" (System/getProperty file-fallback-property "true")))

(defn- normalize-base-url
  [base-url]
  (some-> base-url str/trim (str/replace #"/+$" "")))

(defn- valid-base-url?
  "True when base-url parses as an absolute URL with an http(s) scheme and
   a non-blank host. Rejects bare aliases like 'claude' that would otherwise
   slip through and only fail deep inside http-kit with 'host is null'."
  [base-url]
  (try
    (let [uri    (URI. base-url)
          scheme (some-> (.getScheme uri) str/lower-case)
          host   (.getHost uri)]
      (and scheme
           (contains? #{"http" "https"} scheme)
           (some? host)
           (not (str/blank? host))))
    (catch Exception _ false)))

(defn resolve-llm-config
  "Resolve {:base-url :api-key :model} from env, then ~/.noumenon/credentials
   if the file-fallback gate is open. Returns the map; callers that need
   base-url and api-key must check those keys themselves — `make-messages-fn`
   does that at invocation time so missing values produce a clean message
   naming the variable."
  []
  (let [file-creds (if (file-fallback-enabled?) (util/read-credentials-file) {})
        pick (fn [var-name]
               (or (not-empty (util/env var-name))
                   (not-empty (get file-creds var-name))))]
    {:base-url (normalize-base-url (pick "NOUMENON_LLM_BASE_URL"))
     :api-key  (pick "NOUMENON_LLM_API_KEY")
     :model    (pick "NOUMENON_LLM_MODEL")}))

(defn- missing-var-message
  [var-name]
  (str "Missing " var-name ". Set the env var, "
       "or add `" var-name "=<value>` to ~/.noumenon/credentials and run `noum setup` "
       "(file is consulted automatically when present)."))

(defn- require-base-url! [base-url]
  (cond
    (str/blank? base-url)
    (let [msg (missing-var-message "NOUMENON_LLM_BASE_URL")]
      (throw (ex-info msg {:env-var "NOUMENON_LLM_BASE_URL"
                           :status 400
                           :message msg})))

    (not (valid-base-url? base-url))
    (let [msg (str "Invalid NOUMENON_LLM_BASE_URL: " (pr-str base-url)
                   ". Expected an absolute URL with scheme and host "
                   "(e.g. https://api.anthropic.com).")]
      (throw (ex-info msg {:env-var "NOUMENON_LLM_BASE_URL"
                           :status 400
                           :message msg
                           :value base-url})))

    :else base-url))

(defn- require-api-key! [api-key]
  (when (str/blank? api-key)
    (let [msg (missing-var-message "NOUMENON_LLM_API_KEY")]
      (throw (ex-info msg {:env-var "NOUMENON_LLM_API_KEY"
                           :status 400
                           :message msg}))))
  api-key)

(defn- require-model! [model]
  (when (str/blank? model)
    (let [msg (str "No model selected. Pass --model, set NOUMENON_LLM_MODEL, "
                   "or add NOUMENON_LLM_MODEL=<id> to ~/.noumenon/credentials.")]
      (throw (ex-info msg {:env-var "NOUMENON_LLM_MODEL"
                           :status 400
                           :message msg}))))
  model)

(defn- base-url-host
  "Extract the host[:port] portion of a base URL, for provenance tagging.
   Falls back to the raw URL on parse failure."
  [base-url]
  (try
    (let [uri  (URI. base-url)
          host (.getHost uri)
          port (.getPort uri)]
      (cond
        (and host (pos? port)) (str host ":" port)
        host                   host
        :else                  base-url))
    (catch Exception _ base-url)))

;; --- Direct API invocation ---

(def ^:private retryable-status #{429 500 502 503 504})
(def ^:dynamic *max-retries* 3)
(def ^:dynamic *retry-delays-ms* [2000 4000])

(defn- build-api-request
  "Build the JSON request body and http-kit options for the Messages API."
  [messages {:keys [model temperature max-tokens base-url auth-token system]}]
  {:url     (str base-url "/v1/messages")
   :method  :post
   :headers {"Content-Type"     "application/json"
             "x-api-key"        auth-token
             "anthropic-version" "2023-06-01"}
   :body    (json/write-str
             (cond-> {:model model :max_tokens (or max-tokens 4096) :messages messages}
               temperature (assoc :temperature temperature)
               system      (assoc :system [{:type "text" :text system
                                            :cache_control {:type "ephemeral"}}])))
   :timeout 300000})

(defn- parse-api-response
  "Parse a successful API response body into {:text :usage :model :resolved-model}."
  [body start-ms]
  (let [dur-ms  (- (System/currentTimeMillis) start-ms)
        parsed  (json/read-str body :key-fn keyword)
        text    (some #(when (= "text" (:type %)) (:text %)) (:content parsed))
        usage   (:usage parsed)
        in      (:input_tokens usage 0)
        out     (:output_tokens usage 0)
        cached  (:cache_read_input_tokens usage 0)
        created (:cache_creation_input_tokens usage 0)]
    (when-not text
      (log! (str "WARNING: API returned HTTP 200 but no text content"
                 " (stop_reason=" (:stop_reason parsed)
                 " content=" (truncate (pr-str (:content parsed)) 200) ")")))
    {:text           text
     :usage          (cond-> {:input-tokens in :output-tokens out
                              :cost-usd (estimate-cost (:model parsed) in out)
                              :duration-ms dur-ms}
                       (pos? cached)  (assoc :cache-read-tokens cached)
                       (pos? created) (assoc :cache-creation-tokens created))
     :model          (:model parsed)
     :resolved-model (:model parsed)}))

(defn- classify-attempt
  "Classify one HTTP attempt as :ok, :retry, or :fail."
  [{:keys [status error]} attempt]
  (let [retryable?    (or error (retryable-status status))
        last-attempt? (>= attempt *max-retries*)]
    (cond
      (and (or error (not= 200 status)) retryable? (not last-attempt?)) :retry
      (or error (not= 200 status))                                      :fail
      :else                                                             :ok)))

(defn- retry-reason [{:keys [error status]}]
  (if error
    (.getMessage ^Exception error)
    (str "HTTP " status)))

(defn- failure-ex [{:keys [error status]} attempt]
  (if error
    (ex-info (str "API request failed: " (.getMessage ^Exception error))
             {:error error :attempts attempt})
    (ex-info (str "API error: HTTP " status)
             {:status status :attempts attempt})))

(defn invoke-api
  "Invoke Anthropic Messages API directly via http-kit.
   `messages` is [{:role \"user\"/\"assistant\" :content string} ...].
   Returns {:text string :usage {:input-tokens n :output-tokens m} :model string}.
   Makes up to 3 attempts on transient errors (429, 5xx, connection failures).
   Throws ex-info on persistent HTTP errors."
  [messages opts]
  (let [req      (build-api-request messages opts)
        start-ms (System/currentTimeMillis)]
    (loop [attempt 1]
      (let [resp (deref (http/request req))]
        (case (classify-attempt resp attempt)
          :retry (do (log! (str "  Retry " attempt "/" *max-retries* ": " (retry-reason resp)))
                     (Thread/sleep (get *retry-delays-ms* (dec attempt) 4000))
                     (recur (inc attempt)))
          :fail  (throw (failure-ex resp attempt))
          :ok    (parse-api-response (:body resp) start-ms))))))

;; --- Factory ---

(defn make-messages-fn
  "Create an invoke function for the configured endpoint.
   Returns (fn [messages & [opts]]) where messages is
   [{:role \"user\"/\"assistant\" :content string} ...].
   Optional opts map supports :system (string) for prompt caching.
   Resolves base URL and API key from env / ~/.noumenon/credentials at call time."
  [{:keys [model temperature max-tokens]}]
  (let [{:keys [base-url api-key]} (resolve-llm-config)
        base-url (require-base-url! base-url)
        api-key  (require-api-key!  api-key)]
    (fn invoke
      ([messages] (invoke messages nil))
      ([messages opts]
       (invoke-api messages (cond-> {:model       model
                                     :temperature temperature
                                     :max-tokens  max-tokens
                                     :base-url    base-url
                                     :auth-token  api-key}
                              (:system opts) (assoc :system (:system opts))))))))

(defn wrap-as-prompt-fn
  "Wrap a messages-based invoke fn into a string-prompt fn.
   Returns (fn [prompt-string] -> {:text :usage :resolved-model})."
  [invoke-fn]
  (fn [prompt]
    (invoke-fn [{:role "user" :content prompt}])))

(defn make-messages-fn-from-opts
  "Build a messages-based invoke-fn from caller opts.
   `opts` is {:model :temperature :max-tokens}; :model falls back to NOUMENON_LLM_MODEL.
   Returns {:invoke-fn fn, :model-id string, :provider string}.
   :provider is the base-URL host (e.g. \"api.anthropic.com\") — kept as a
   provenance tag for analyze/synthesize transactions, NOT used to dispatch."
  [{:keys [model temperature max-tokens]}]
  (let [config   (resolve-llm-config)
        _        (require-base-url! (:base-url config))
        _        (require-api-key!  (:api-key  config))
        model-id (require-model! (or model (:model config)))
        host     (base-url-host (:base-url config))]
    {:invoke-fn  (make-messages-fn (cond-> {:model model-id}
                                     temperature (assoc :temperature temperature)
                                     max-tokens  (assoc :max-tokens max-tokens)))
     :model-id   model-id
     :provider   host}))

(defn wrap-as-prompt-fn-from-opts
  "Build a prompt-fn (string->result) from caller opts.
   Returns {:prompt-fn fn, :model-id string, :provider string}."
  [opts]
  (let [{:keys [invoke-fn] :as resolved} (make-messages-fn-from-opts opts)]
    (-> resolved
        (assoc :prompt-fn (wrap-as-prompt-fn invoke-fn))
        (dissoc :invoke-fn))))

(defn make-isolated-prompt-fn
  "Build an isolated prompt-fn for benchmark raw-mode calls."
  [opts]
  (:prompt-fn (wrap-as-prompt-fn-from-opts opts)))
