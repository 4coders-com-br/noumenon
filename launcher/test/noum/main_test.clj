(ns noum.main-test
  "Unit tests for noum.main dispatch behavior. Run via:
       bb -cp src:resources:test -m clojure.test/run-tests noum.main-test"
  (:require [clojure.test :refer [deftest is testing]]
            [noum.api :as api]
            [noum.main]))

(deftest update-is-in-progress-commands
  (testing "Without `update` in progress-commands, `noum update <repo> --analyze`
            posts to /api/update with no Accept: text/event-stream header and
            no on-progress callback, so the launcher prints nothing for the
            entire duration of a fresh sync + LLM analysis pass. The user sees
            only the JRE-selection line and then dead silence (often many
            minutes) — looks identical to a hang. Other long-running pipeline
            commands (import, analyze, enrich, digest, bench, introspect) are
            in the set; update must join them."
    (let [pc @#'noum.main/progress-commands]
      (is (contains? pc "update")
          (str "expected \"update\" in progress-commands; got: " (pr-str pc))))))

(deftest watch-loop-requests-sse-with-on-progress
  (testing "noum watch <repo> --analyze suffers the same silent-update
            problem as `noum update`: each polling iteration calls
            /api/update synchronously with no on-progress callback, so
            an iteration that triggers a fresh import + LLM analysis
            blocks for minutes with no per-poll feedback. watch-loop!
            must pass an on-progress function to api/post! so each
            in-flight iteration streams events instead of going silent.
            The end-of-iteration `Updated: ...` summary only prints
            AFTER the call returns — useless during the long pass."
    (let [watch-loop! @#'noum.main/watch-loop!
          captured    (atom [])
          seen        (promise)]
      (with-redefs [api/post!
                    (fn
                      ([_conn _path _body]
                       (swap! captured conj {:arity 3})
                       (deliver seen :got-it)
                       (throw (ex-info "stop-loop" {})))
                      ([_conn _path _body on-progress]
                       (swap! captured conj {:arity 4 :on-progress on-progress})
                       (deliver seen :got-it)
                       (throw (ex-info "stop-loop" {}))))
                    noum.tui.core/eprintln    (fn [_] nil)
                    noum.tui.core/interactive? (constantly true)
                    noum.tui.spinner/start    (fn [_] {:stop (fn [_]) :fail (fn [_])})]
        (let [f (future (try
                          (watch-loop! {:conn       {:host "x"}
                                        :repo-path  "/tmp/x"
                                        :body       {:repo_path "/tmp/x" :analyze true}
                                        :interval-s 1})
                          (catch Exception _ :ok)))]
          (deref seen 2000 :timeout)
          (future-cancel f)))
      (let [calls @captured]
        (is (seq calls) "watch-loop! must invoke api/post! at least once")
        (when (seq calls)
          (let [{:keys [arity on-progress]} (first calls)]
            (is (= 4 arity)
                "watch-loop! must use the 4-arity api/post! (with on-progress) so SSE flows")
            (is (fn? on-progress)
                "the on-progress argument must be a fn (a TUI progress/spinner sink)")))))))
