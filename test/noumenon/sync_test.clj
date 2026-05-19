(ns noumenon.sync-test
  (:require [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is testing]]
            [datomic.client.api :as d]
            [noumenon.analyze :as analyze]
            [noumenon.calls :as calls]
            [noumenon.files :as files]
            [noumenon.git :as git]
            [noumenon.imports :as imports]
            [noumenon.sync :as sync]
            [noumenon.test-helpers :as th]))

(deftest valid-sha?-test
  (testing "accepts valid 40-char hex SHA"
    (is (sync/valid-sha? "abc123def456789012345678901234567890abcd"))
    (is (sync/valid-sha? "0000000000000000000000000000000000000000")))
  (testing "rejects invalid SHAs"
    (is (not (sync/valid-sha? nil)))
    (is (not (sync/valid-sha? "")))
    (is (not (sync/valid-sha? "abc123")))
    (is (not (sync/valid-sha? "ABCDEF0000000000000000000000000000000000")))
    (is (not (sync/valid-sha? "abc123def456789012345678901234567890abcd; rm -rf /")))
    (is (not (sync/valid-sha? "--option-injection")))))

(def ^:private test-sha "abc123def456789012345678901234567890abcd")

(deftest changed-files-rename-test
  (testing "rename lines produce :deleted for old path and :added for new path"
    (with-redefs [shell/sh (constantly {:exit 0 :out "R100\told/file.clj\tnew/file.clj\n"})]
      (let [result (sync/changed-files "/tmp" test-sha)]
        (is (= ["old/file.clj"] (:deleted result)))
        (is (= ["new/file.clj"] (:added result)))
        (is (= [] (:modified result))))))
  (testing "mixed statuses parsed correctly"
    (with-redefs [shell/sh (constantly
                            {:exit 0
                             :out (str "A\tsrc/new.clj\n"
                                       "M\tsrc/changed.clj\n"
                                       "D\tsrc/gone.clj\n"
                                       "R100\tsrc/old.clj\tsrc/renamed.clj\n")})]
      (let [result (sync/changed-files "/tmp" test-sha)]
        (is (= ["src/new.clj" "src/renamed.clj"] (:added result)))
        (is (= ["src/changed.clj"] (:modified result)))
        (is (= ["src/gone.clj" "src/old.clj"] (:deleted result))))))
  (testing "copy lines produce :added for new path only"
    (with-redefs [shell/sh (constantly {:exit 0 :out "C100\tsrc/orig.clj\tsrc/copy.clj\n"})]
      (let [result (sync/changed-files "/tmp" test-sha)]
        (is (= ["src/copy.clj"] (:added result)))
        (is (= [] (:deleted result)))
        (is (= [] (:modified result))))))
  (testing "invalid SHA returns nil"
    (is (nil? (sync/changed-files "/tmp" "not-a-sha"))))
  (testing "well-formed SHA that does not resolve in the repo throws — guards
            against a typo'd basis_sha producing a silent empty diff"
    (with-redefs [shell/sh (constantly {:exit 128 :out "" :err "fatal: bad object"})]
      (let [thrown (try (sync/changed-files "/tmp" test-sha)
                        (catch clojure.lang.ExceptionInfo e e))]
        (is (instance? clojure.lang.ExceptionInfo thrown))
        (is (= 400 (:status (ex-data thrown))))
        (is (re-find #"does not resolve" (.getMessage thrown)))))))

(deftest deleted-file-tx-test
  (testing "produces tombstone upsert keyed on :file/path identity"
    (is (= {:file/path "src/gone.clj" :file/deleted? true}
           (sync/deleted-file-tx "src/gone.clj")))))

(deftest head-and-branch-tx-test
  (testing "nil sha returns nil"
    (is (nil? (sync/head-and-branch-tx {:repo-uri "u" :sha nil :branch-name "main"}))))
  (testing "no branch-name → only head-sha tx, no :repo/branch pointer"
    (let [tx (sync/head-and-branch-tx {:repo-uri "u" :sha "abc"})]
      (is (= 1 (count tx)))
      (is (= {:db/id "repo" :repo/uri "u" :repo/head-sha "abc"} (first tx)))))
  (testing "branch-name present → repo gets pointer + branch entity transacted"
    (let [tx (sync/head-and-branch-tx
              {:repo-uri    "u"
               :sha         "abc"
               :branch-name "main"
               :branch-kind :trunk
               :branch-vcs  :git})]
      (is (= 2 (count tx)))
      (is (= {:db/id "repo" :repo/uri "u" :repo/head-sha "abc" :repo/branch "branch"}
             (first tx)))
      (is (= {:db/id        "branch"
              :branch/repo  "repo"
              :branch/name  "main"
              :branch/kind  :trunk
              :branch/vcs   :git}
             (second tx)))))
  (testing "delta opts populate basis-sha + parent fields on the branch entity"
    (let [tx (sync/head-and-branch-tx
              {:repo-uri        "u"
               :sha             "dev"
               :branch-name     "feat/x"
               :branch-kind     :feature
               :branch-vcs      :git
               :basis-sha       "trunk-sha"
               :parent-host     "noumenon.example"
               :parent-db-name  "myrepo"})
          branch (second tx)]
      (is (= "trunk-sha" (:branch/basis-sha branch)))
      (is (= "noumenon.example" (:branch/parent-host branch)))
      (is (= "myrepo" (:branch/parent-db-name branch)))))
  (testing "delta opts omitted on trunk → no basis/parent attrs emitted"
    (let [branch (-> (sync/head-and-branch-tx
                      {:repo-uri "u" :sha "abc" :branch-name "main"
                       :branch-kind :trunk :branch-vcs :git})
                     second)]
      (is (not (contains? branch :branch/basis-sha)))
      (is (not (contains? branch :branch/parent-host)))
      (is (not (contains? branch :branch/parent-db-name))))))

(deftest update-head-and-branch!-is-idempotent
  (testing "Re-running update-head-and-branch! on the same repo+branch
            does not trip :branch/repo+name unique-conflict — the existing
            entity is upserted in place rather than recreated as a new tempid"
    (let [conn (th/make-test-conn "head-branch-idempotent")
          opts {:repo-uri    "/Users/leif/Code/noumenon"
                :sha         "abc123def456789012345678901234567890abcd"
                :branch-name "feat/branch-aware-graph"
                :branch-kind :feature
                :branch-vcs  :git}
          eid1 (sync/update-head-and-branch! conn opts)
          eid2 (sync/update-head-and-branch! conn opts)
          eid3 (sync/update-head-and-branch! conn (assoc opts :sha "ffffffff00000000000000000000000000000fff"))]
      (is (some? eid1) "first call returns the new repo eid")
      (is (= eid1 eid2) "second call (no changes) returns the same eid, no exception")
      (is (= eid1 eid3) "third call with a different HEAD SHA still upserts in place")
      (testing "trunk-side update with a different branch creates a second branch entity"
        (let [other-eid (sync/update-head-and-branch! conn (assoc opts :branch-name "main" :branch-kind :trunk))]
          (is (= eid1 other-eid) "repo eid stable across branch switches")
          (let [branches (d/q '[:find ?n :where [?b :branch/name ?n]] (d/db conn))]
            (is (= #{"feat/branch-aware-graph" "main"} (into #{} (map first) branches))
                "both branches are recorded in the DB")))))))

(deftest update-repo!-invokes-progress-fn-through-pipeline-stages
  (testing "When sync/update-repo! is called with :progress-fn, the callback
            must be threaded into the underlying stages (git/import-commits!,
            imports/enrich-repo!, analyze/analyze-repo!) so the daemon can
            stream SSE progress to clients. Without this plumbing, /api/update
            looks identical to a hang during a fresh sync + --analyze pass:
            the HTTP request blocks for minutes with no events."
    (let [conn         (th/make-test-conn "update-repo-progress")
          repo-path    "/tmp/nonexistent-noumenon-progress-test"
          repo-uri     repo-path
          import-pfn   (atom nil)
          enrich-pfn   (atom nil)
          analyze-pfn  (atom nil)]
      (with-redefs [git/head-sha               (constantly "abc123def456789012345678901234567890abcd")
                    git/current-branch-name    (constantly "main")
                    git/classify-branch-kind   (constantly :trunk)
                    git/import-commits!        (fn [_conn _repo _uri & [progress-fn]]
                                                 (reset! import-pfn progress-fn)
                                                 {:commits-imported 0 :commits-skipped 0 :elapsed-ms 0})
                    files/import-files!        (fn [& _]
                                                 {:files-imported 0 :files-skipped 0 :dirs-imported 0})
                    imports/enrich-repo!       (fn [_conn _repo opts]
                                                 (reset! enrich-pfn (:progress-fn opts))
                                                 {:files-processed 0 :imports-resolved 0})
                    analyze/analyze-repo!      (fn [_conn _repo _invoke opts]
                                                 (reset! analyze-pfn (:progress-fn opts))
                                                 {:files-analyzed 0 :files-promoted 0
                                                  :files-skipped 0 :files-errored 0
                                                  :files-parse-errored 0
                                                  :total-usage {:input-tokens 0 :output-tokens 0
                                                                :cost-usd 0 :duration-ms 0}})
                    calls/resolve-calls!       (fn [_conn] {:resolved 0})]
        (sync/update-repo! conn repo-path repo-uri
                           {:concurrency 1
                            :analyze-concurrency 1
                            :analyze?    true
                            :invoke-llm  (fn [_] {:text "{}" :usage {}})
                            :meta-db     (d/db conn)
                            :model-id    "stub"
                            :provider    "stub"
                            :progress-fn (fn [_evt] nil)}))
      (is (fn? @import-pfn)
          "git/import-commits! must receive a non-nil progress-fn forwarded from update-repo!")
      (is (fn? @enrich-pfn)
          "imports/enrich-repo! must receive a non-nil :progress-fn forwarded from update-repo!")
      (is (fn? @analyze-pfn)
          "analyze/analyze-repo! must receive a non-nil :progress-fn forwarded from update-repo!"))))
