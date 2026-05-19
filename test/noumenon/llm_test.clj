(ns noumenon.llm-test
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [noumenon.llm :as llm]
            [noumenon.util :as util]
            [org.httpkit.client]))

;; --- Tier 0: invoke-api with mock HTTP ---

(deftest invoke-api-success
  (testing "successful API call returns parsed response"
    (let [response-body (json/write-str
                         {:content [{:type "text" :text "The correct answer is (B)"}]
                          :usage {:input_tokens 500 :output_tokens 20}
                          :model "claude-3-5-sonnet-20241022"})
          mock-request  (fn [opts]
                          (is (= :post (:method opts)))
                          (is (re-find #"/v1/messages" (:url opts)))
                          (let [body (json/read-str (:body opts) :key-fn keyword)]
                            (is (= 0.1 (:temperature body)))
                            (is (= 128 (:max_tokens body)))
                            (is (= [{:role "user" :content "test prompt"}]
                                   (:messages body))))
                          (delay {:status 200 :body response-body :error nil}))]
      (with-redefs [org.httpkit.client/request mock-request]
        (let [result (llm/invoke-api [{:role "user" :content "test prompt"}]
                                     {:model "claude-3-5-sonnet-20241022"
                                      :temperature 0.1
                                      :max-tokens 128
                                      :base-url "https://api.example.com"
                                      :auth-token "test-token"})]
          (is (= "The correct answer is (B)" (:text result)))
          (is (= 500 (get-in result [:usage :input-tokens])))
          (is (= 20 (get-in result [:usage :output-tokens]))))))))

(deftest invoke-api-defaults-max-tokens
  (testing "max_tokens defaults to 4096 when not specified"
    (let [response-body (json/write-str
                         {:content [{:type "text" :text "ok"}]
                          :usage {:input_tokens 10 :output_tokens 5}
                          :model "m"})
          captured-body (atom nil)
          mock-request  (fn [opts]
                          (reset! captured-body (json/read-str (:body opts) :key-fn keyword))
                          (delay {:status 200 :body response-body :error nil}))]
      (with-redefs [org.httpkit.client/request mock-request]
        (llm/invoke-api [{:role "user" :content "test"}]
                        {:model "m" :temperature 0.1
                         :base-url "https://x" :auth-token "t"})
        (is (= 4096 (:max_tokens @captured-body)))))))

(deftest invoke-api-429-throws
  (testing "HTTP 429 throws ex-info with status code"
    (binding [llm/*max-retries* 1]
      (let [mock-request (fn [_opts]
                           (delay {:status 429 :body "rate limited" :error nil}))]
        (with-redefs [org.httpkit.client/request mock-request]
          (is (thrown-with-msg?
               clojure.lang.ExceptionInfo
               #"API error: HTTP 429"
               (llm/invoke-api [{:role "user" :content "test"}]
                               {:model "m" :temperature 0.1
                                :max-tokens 128 :base-url "https://x"
                                :auth-token "t"}))))))))

(deftest invoke-api-500-throws
  (testing "HTTP 500 throws ex-info with status code after retries exhausted"
    (binding [llm/*max-retries* 1]
      (let [mock-request (fn [_opts]
                           (delay {:status 500 :body "server error" :error nil}))]
        (with-redefs [org.httpkit.client/request mock-request]
          (is (thrown-with-msg?
               clojure.lang.ExceptionInfo
               #"API error: HTTP 500"
               (llm/invoke-api [{:role "user" :content "test"}]
                               {:model "m" :temperature 0.1
                                :max-tokens 128 :base-url "https://x"
                                :auth-token "t"}))))))))

(deftest invoke-api-error-omits-body-from-ex-data
  (testing "error ex-data contains status but not response body"
    (binding [llm/*max-retries* 1]
      (let [mock-request (fn [_opts]
                           (delay {:status 500 :body "sensitive error details" :error nil}))]
        (with-redefs [org.httpkit.client/request mock-request]
          (try
            (llm/invoke-api [{:role "user" :content "test"}]
                            {:model "m" :temperature 0.1
                             :max-tokens 128 :base-url "https://x"
                             :auth-token "t"})
            (catch clojure.lang.ExceptionInfo e
              (is (= 500 (:status (ex-data e))))
              (is (nil? (:body (ex-data e)))))))))))

(deftest invoke-api-connection-error-throws
  (testing "connection error throws ex-info after retries exhausted"
    (binding [llm/*max-retries* 1]
      (let [mock-request (fn [_opts]
                           (delay {:status nil :body nil
                                   :error (Exception. "connection refused")}))]
        (with-redefs [org.httpkit.client/request mock-request]
          (is (thrown-with-msg?
               clojure.lang.ExceptionInfo
               #"API request failed: connection refused"
               (llm/invoke-api [{:role "user" :content "test"}]
                               {:model "m" :temperature 0.1
                                :max-tokens 128 :base-url "https://x"
                                :auth-token "t"}))))))))

;; --- Usage tracking ---

(deftest sum-usage-nil-handling
  (testing "nil arguments treated as zero-usage"
    (is (= llm/zero-usage (llm/sum-usage nil nil)))
    (is (= {:input-tokens 10 :output-tokens 5 :cost-usd 0.1 :duration-ms 100}
           (llm/sum-usage {:input-tokens 10 :output-tokens 5 :cost-usd 0.1 :duration-ms 100} nil)))
    (is (= {:input-tokens 10 :output-tokens 5 :cost-usd 0.1 :duration-ms 100}
           (llm/sum-usage nil {:input-tokens 10 :output-tokens 5 :cost-usd 0.1 :duration-ms 100})))))

(deftest sum-usage-normal
  (testing "sums two usage maps"
    (let [result (llm/sum-usage {:input-tokens 10 :output-tokens 5 :cost-usd 0.1 :duration-ms 100}
                                {:input-tokens 20 :output-tokens 10 :cost-usd 0.2 :duration-ms 200})]
      (is (= 30 (:input-tokens result)))
      (is (= 15 (:output-tokens result)))
      (is (= 300 (:duration-ms result)))
      (is (< (abs (- 0.3 (:cost-usd result))) 1e-10)))))

;; --- Pricing ---

(deftest estimate-cost-known-models
  (testing "known model prefixes return non-zero cost"
    (is (pos? (llm/estimate-cost "claude-sonnet-4-6-20250514" 1000 100)))
    (is (pos? (llm/estimate-cost "claude-haiku-4-5-20251001" 1000 100)))))

(deftest estimate-cost-unknown-model
  (testing "unknown models return zero cost"
    (is (= 0.0 (llm/estimate-cost "gpt-4" 1000 100)))
    (is (= 0.0 (llm/estimate-cost nil 1000 100)))))

;; --- Config resolution ---

(defn- with-env [m thunk]
  (with-redefs [util/env (fn [k] (get m k))
                util/read-credentials-file (fn
                                             ([] {})
                                             ([_] {}))]
    (thunk)))

(defn- with-env+file [env file thunk]
  (with-redefs [util/env (fn [k] (get env k))
                util/read-credentials-file (fn
                                             ([] file)
                                             ([_] file))]
    (thunk)))

(deftest resolve-llm-config-env-only
  (testing "env vars populate {:base-url :api-key :model}"
    (with-env {"NOUMENON_LLM_BASE_URL" "https://api.anthropic.com/"
               "NOUMENON_LLM_API_KEY"  "k"
               "NOUMENON_LLM_MODEL"    "claude-sonnet-4-6"}
      (fn []
        (let [c (llm/resolve-llm-config)]
          (is (= "https://api.anthropic.com" (:base-url c)))
          (is (= "k" (:api-key c)))
          (is (= "claude-sonnet-4-6" (:model c))))))))

(deftest resolve-llm-config-falls-back-to-file
  (testing "credentials file fills in missing env values when gate is open"
    (System/clearProperty "noumenon.allow-file-credentials")
    (with-env+file {} {"NOUMENON_LLM_BASE_URL" "https://openrouter.ai/api/v1"
                       "NOUMENON_LLM_API_KEY"  "or-key"
                       "NOUMENON_LLM_MODEL"    "anthropic/claude-sonnet-4-5"}
      (fn []
        (let [c (llm/resolve-llm-config)]
          (is (= "https://openrouter.ai/api/v1" (:base-url c)))
          (is (= "or-key" (:api-key c)))
          (is (= "anthropic/claude-sonnet-4-5" (:model c))))))))

(deftest resolve-llm-config-env-overrides-file
  (testing "env wins over file"
    (System/clearProperty "noumenon.allow-file-credentials")
    (with-env+file {"NOUMENON_LLM_API_KEY" "from-env"}
      {"NOUMENON_LLM_BASE_URL" "https://api.anthropic.com"
       "NOUMENON_LLM_API_KEY"  "from-file"}
      (fn []
        (let [c (llm/resolve-llm-config)]
          (is (= "https://api.anthropic.com" (:base-url c)))
          (is (= "from-env" (:api-key c))))))))

(deftest resolve-llm-config-file-fallback-gate-disabled
  (testing "file is ignored when JVM property is \"false\""
    (try
      (System/setProperty "noumenon.allow-file-credentials" "false")
      (with-env+file {} {"NOUMENON_LLM_BASE_URL" "https://x"
                         "NOUMENON_LLM_API_KEY"  "should-be-ignored"}
        (fn []
          (let [c (llm/resolve-llm-config)]
            (is (nil? (:base-url c)))
            (is (nil? (:api-key c))))))
      (finally (System/clearProperty "noumenon.allow-file-credentials")))))

(deftest make-messages-fn-from-opts-missing-base-url
  (testing "missing NOUMENON_LLM_BASE_URL produces a clean error"
    (System/clearProperty "noumenon.allow-file-credentials")
    (with-env {"NOUMENON_LLM_API_KEY" "k" "NOUMENON_LLM_MODEL" "m"}
      (fn []
        (try
          (llm/make-messages-fn-from-opts {})
          (is false "expected exception")
          (catch clojure.lang.ExceptionInfo e
            (is (re-find #"NOUMENON_LLM_BASE_URL" (.getMessage e)))
            (is (= "NOUMENON_LLM_BASE_URL" (:env-var (ex-data e))))
            (is (= 400 (:status (ex-data e)))
                "Missing config is a 4xx user error, not a 500 server error")))))))

(deftest make-messages-fn-from-opts-invalid-base-url
  (testing "NOUMENON_LLM_BASE_URL must be an absolute URL with scheme+host.
            Saving a bare alias like 'claude' used to slip through validation
            and only fail deep inside http-kit with 'host is null:
            claude/v1/messages', which the daemon rewrote as a generic 500
            'Internal server error'. The launcher then showed the user
            'Error: Internal server error' with no clue that the credentials
            file was at fault. Validate at config-resolution time so the
            error names the env var and the offending value."
    (System/clearProperty "noumenon.allow-file-credentials")
    (doseq [bad ["claude" "anthropic" "api.anthropic.com" "/v1/messages"
                 "http:" "ftp://example.com" "://example.com"]]
      (with-env {"NOUMENON_LLM_BASE_URL" bad
                 "NOUMENON_LLM_API_KEY"  "k"
                 "NOUMENON_LLM_MODEL"    "m"}
        (fn []
          (try
            (llm/make-messages-fn-from-opts {})
            (is false (str "expected exception for invalid base-url: " (pr-str bad)))
            (catch clojure.lang.ExceptionInfo e
              (is (re-find #"NOUMENON_LLM_BASE_URL" (.getMessage e))
                  (str "error should name the env var; got: " (.getMessage e)))
              (is (= "NOUMENON_LLM_BASE_URL" (:env-var (ex-data e))))
              (is (= 400 (:status (ex-data e)))
                  "Bad credentials are a 4xx, not 500"))))))))

(deftest make-messages-fn-from-opts-valid-base-urls-pass-validation
  (testing "valid http(s) URLs survive validation — anchor for the bad-url test"
    (System/clearProperty "noumenon.allow-file-credentials")
    (doseq [good ["https://api.anthropic.com"
                  "https://api.anthropic.com/"
                  "http://localhost:8080"
                  "https://openrouter.ai/api/v1"
                  "https://example.com/path/segments"]]
      (with-env {"NOUMENON_LLM_BASE_URL" good
                 "NOUMENON_LLM_API_KEY"  "k"
                 "NOUMENON_LLM_MODEL"    "m"}
        (fn []
          (is (some? (llm/make-messages-fn-from-opts {}))
              (str "expected " (pr-str good) " to pass validation")))))))

(deftest make-messages-fn-from-opts-missing-api-key
  (testing "missing NOUMENON_LLM_API_KEY produces a clean error"
    (System/clearProperty "noumenon.allow-file-credentials")
    (with-env {"NOUMENON_LLM_BASE_URL" "https://api.anthropic.com" "NOUMENON_LLM_MODEL" "m"}
      (fn []
        (try
          (llm/make-messages-fn-from-opts {})
          (is false "expected exception")
          (catch clojure.lang.ExceptionInfo e
            (is (re-find #"NOUMENON_LLM_API_KEY" (.getMessage e)))
            (is (= "NOUMENON_LLM_API_KEY" (:env-var (ex-data e))))
            (is (= 400 (:status (ex-data e)))
                "Missing config is a 4xx, not 500")))))))

(deftest make-messages-fn-from-opts-missing-model
  (testing "missing model produces a clean error mentioning --model"
    (System/clearProperty "noumenon.allow-file-credentials")
    (with-env {"NOUMENON_LLM_BASE_URL" "https://api.anthropic.com"
               "NOUMENON_LLM_API_KEY"  "k"}
      (fn []
        (try
          (llm/make-messages-fn-from-opts {})
          (is false "expected exception")
          (catch clojure.lang.ExceptionInfo e
            (is (re-find #"No model selected" (.getMessage e)))
            (is (re-find #"--model" (.getMessage e)))
            (is (= 400 (:status (ex-data e)))
                "Missing config is a 4xx, not 500")))))))

(deftest make-messages-fn-from-opts-opt-overrides-env
  (testing "opts :model overrides NOUMENON_LLM_MODEL"
    (System/clearProperty "noumenon.allow-file-credentials")
    (with-env {"NOUMENON_LLM_BASE_URL" "https://api.anthropic.com"
               "NOUMENON_LLM_API_KEY"  "k"
               "NOUMENON_LLM_MODEL"    "default-model"}
      (fn []
        (let [r (llm/make-messages-fn-from-opts {:model "override-model"})]
          (is (= "override-model" (:model-id r)))
          (is (= "api.anthropic.com" (:provider r))))))))

(deftest wrap-as-prompt-fn-from-opts-shape
  (testing "wrap-as-prompt-fn-from-opts returns prompt-fn, model-id, provider"
    (System/clearProperty "noumenon.allow-file-credentials")
    (with-env {"NOUMENON_LLM_BASE_URL" "https://api.anthropic.com"
               "NOUMENON_LLM_API_KEY"  "k"}
      (fn []
        (let [r (llm/wrap-as-prompt-fn-from-opts {:model "m"})]
          (is (fn? (:prompt-fn r)))
          (is (= "m" (:model-id r)))
          (is (str/includes? (:provider r) "anthropic")))))))
