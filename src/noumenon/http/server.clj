(ns noumenon.http.server
  "HTTP daemon lifecycle — server-atom, daemon.edn write/delete, and
   resolve-server-config. `start!` and `stop!` are the public surface."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [noumenon.db :as db]
            [noumenon.http.routes :as routes]
            [noumenon.llm :as llm]
            [noumenon.util :as util :refer [log!]]
            [org.httpkit.server :as server])
  (:import [java.lang ProcessHandle]))

(defonce ^:private server-atom (atom nil))

(defn- write-daemon-file!
  "Write daemon.edn with port, pid, and start time. Owner-only permissions."
  [port]
  (let [home-dir    (io/file (System/getProperty "user.home") ".noumenon")
        daemon-file (io/file home-dir "daemon.edn")]
    (.mkdirs home-dir)
    (spit daemon-file
          (pr-str {:port       port
                   :pid        (.pid (ProcessHandle/current))
                   :started-at (System/currentTimeMillis)}))
    (try
      (let [ok? (and (.setReadable daemon-file false false)
                     (.setWritable daemon-file false false)
                     (.setReadable daemon-file true false)
                     (.setWritable daemon-file true false))]
        (when-not ok?
          (log! "daemon/warn" "Could not set owner-only permissions on daemon.edn")))
      (catch Exception _
        (log! "daemon/warn" "Could not set permissions on daemon.edn")))))

(defn- delete-daemon-file! []
  (let [f (io/file (System/getProperty "user.home") ".noumenon" "daemon.edn")]
    (when (.exists f) (.delete f))))

(defn- resolve-server-config
  "Merge CLI flags with env-var fallbacks. CLI flags take precedence."
  [{:keys [port bind db-dir model token read-only
           max-ask-sessions log-format webhook-secret poll-interval]}]
  (let [resolved-token   (not-empty (or token (util/env "NOUMENON_TOKEN")))
        resolved-bind    (or bind (System/getenv "NOUMENON_BIND") "127.0.0.1")
        resolved-port    (or port (util/env-int "NOUMENON_PORT") 0)]
    {:port               resolved-port
     :bind               resolved-bind
     :db-dir             (or db-dir (System/getenv "NOUMENON_DB_DIR")
                             (util/resolve-db-dir {}))
     :model              (or model (System/getenv "NOUMENON_LLM_MODEL"))
     :token              resolved-token
     :read-only          (or read-only (util/env-bool "NOUMENON_READ_ONLY"))
     :max-ask-sessions   (or max-ask-sessions (util/env-int "NOUMENON_MAX_ASK_SESSIONS") 50)
     :log-format         (or log-format (System/getenv "NOUMENON_LOG_FORMAT") "text")
     :webhook-secret     (or webhook-secret (util/env "NOUMENON_WEBHOOK_SECRET"))
     :poll-interval      (or poll-interval (util/env-int "NOUMENON_POLL_INTERVAL") 5)
     :started-at         (System/currentTimeMillis)}))

(defn start!
  "Start the HTTP daemon on the given port (0 = auto-assign).
   Returns the actual port."
  [opts]
  (when @server-atom
    (throw (ex-info "Daemon already running" {})))
  (let [{:keys [bind token] :as config} (resolve-server-config opts)
        loopback?   (= bind "127.0.0.1")
        _ (when (and (not loopback?) (str/blank? token))
            (throw (ex-info (str "Cannot bind to " bind " without --token or NOUMENON_TOKEN. "
                                 "Remote access requires authentication.")
                            {})))
        _ (when-not loopback?
            ;; Hosted / shared-service deployment: refuse to consult
            ;; ~/.noumenon/credentials. Operator must supply the LLM
            ;; credentials via env vars on the host.
            (System/setProperty "noumenon.allow-file-credentials" "false"))
        llm-config  (llm/resolve-llm-config)
        _ (when (str/blank? (:base-url llm-config))
            (throw (ex-info (str "Missing NOUMENON_LLM_BASE_URL. Set it on the host before starting the daemon"
                                 (when loopback? " (or add it to ~/.noumenon/credentials and run `noum setup`)")
                                 ".")
                            {:env-var "NOUMENON_LLM_BASE_URL"})))
        _ (when (str/blank? (:api-key llm-config))
            (throw (ex-info (str "Missing NOUMENON_LLM_API_KEY. Set it on the host before starting the daemon"
                                 (when loopback? " (or add it to ~/.noumenon/credentials and run `noum setup`)")
                                 ".")
                            {:env-var "NOUMENON_LLM_API_KEY"})))
        meta-conn   (db/ensure-meta-db (:db-dir config))
        config      (assoc config :meta-conn meta-conn)
        handler     (routes/make-handler config)
        srv         (server/run-server handler {:ip bind :port (:port config)})
        actual-port (or (some-> srv meta :local-port)
                        (:port config))]
    (reset! server-atom srv)
    (write-daemon-file! actual-port)
    (when-not loopback?
      (log! "WARNING: Daemon is network-accessible. Use TLS (reverse proxy) for production.")
      (when (str/starts-with? (str/lower-case (:base-url llm-config)) "http://")
        (log! "WARNING: NOUMENON_LLM_BASE_URL uses http://. Upstream LLM traffic will be in cleartext. Prefer https.")))
    (log! (str "Noumenon daemon listening on " bind ":" actual-port))
    actual-port))

(defn stop! []
  (when-let [srv @server-atom]
    (srv)
    (reset! server-atom nil)
    (delete-daemon-file!)
    (log! "Noumenon daemon stopped")))
