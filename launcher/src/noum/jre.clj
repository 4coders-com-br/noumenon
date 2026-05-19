(ns noum.jre
  "Auto-download and manage JRE from Adoptium."
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [babashka.process :as proc]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [noum.paths :as paths]
            [noum.tui.core :as tui]
            [noum.tui.spinner :as spinner])
  (:import [java.io IOException]
           [java.security MessageDigest]))

(def ^:private jre-version "21")

(defn- detect-os []
  (let [os (str/lower-case (System/getProperty "os.name"))]
    (cond
      (str/includes? os "mac")   "mac"
      (str/includes? os "linux") "linux"
      (str/includes? os "win")   "windows"
      :else (throw (ex-info (str "Unsupported OS: " os) {})))))

(defn- detect-arch []
  (let [arch (System/getProperty "os.arch")]
    (case arch
      "aarch64" "aarch64"
      "arm64"   "aarch64"
      "amd64"   "x64"
      "x86_64"  "x64"
      (throw (ex-info (str "Unsupported architecture: " arch) {})))))

(defn- adoptium-url [os arch]
  (str "https://api.adoptium.net/v3/binary/latest/"
       jre-version "/ga/" os "/" arch
       "/jre/hotspot/normal/eclipse"))

(defn- adoptium-checksum-url [os arch]
  (str "https://api.adoptium.net/v3/checksum/latest/"
       jre-version "/ga/" os "/" arch
       "/jre/hotspot/normal/eclipse"))

(defn- sha256-file [path]
  (let [digest (MessageDigest/getInstance "SHA-256")
        buf    (byte-array 8192)]
    (with-open [in (io/input-stream (str path))]
      (loop []
        (let [n (.read in buf)]
          (when (pos? n)
            (.update digest buf 0 n)
            (recur)))))
    (apply str (map #(format "%02x" (bit-and % 0xff)) (.digest digest)))))

(defn- verify-checksum!
  "Verify archive SHA256 against Adoptium checksum API. Throws on mismatch."
  [archive-path os arch]
  (let [s (spinner/start "Verifying JRE integrity...")]
    (try
      (let [resp     (http/get (adoptium-checksum-url os arch)
                               {:throw false :follow-redirects true})
            expected (when (= 200 (:status resp))
                       (first (re-seq #"[0-9a-f]{64}" (:body resp))))
            actual   (sha256-file archive-path)]
        (cond
          (nil? expected)
          (do ((:stop s) "Warning: could not fetch checksum from Adoptium API")
              (tui/eprintln "  Continuing without checksum verification."))

          (= expected actual)
          ((:stop s) "SHA256 verified")

          :else
          (do ((:fail s))
              (throw (ex-info "SHA256 mismatch -- JRE download may be corrupted. Try again."
                              {:expected expected :actual actual})))))
      (catch Exception e
        (if (= "SHA256 mismatch -- JRE download may be corrupted. Try again." (.getMessage e))
          (throw e)
          (do ((:stop s) "Warning: checksum verification failed")
              (tui/eprintln (str "  " (.getMessage e) ". Continuing without verification."))))))))

(def ^:private min-major-version
  "Minimum Java major version Noumenon's uberjar runs on. Bump when deps.edn
   targets a newer LTS — this is the only system-Java acceptance gate."
  21)

(defn installed?
  "Check if the bundled JRE is available at the expected location."
  []
  (let [java-bin (str (fs/path paths/jre-dir "bin" "java"))]
    (fs/exists? java-bin)))

(defn- parse-major-version
  "Parse the major Java version from `java -version` output. Returns int or nil.
   Handles both modern (\"21.0.4\") and legacy (\"1.8.0_392\") strings — for
   the latter the relevant major is the second segment (8)."
  [version-text]
  (when-let [[_ a b] (some->> version-text (re-find #"version \"(\d+)(?:\.(\d+))?"))]
    (try
      (let [a-int (Integer/parseInt a)]
        (if (and (= 1 a-int) b) (Integer/parseInt b) a-int))
      (catch Exception _ nil))))

(defn- system-java-version
  "Run `java-bin -version` and return its major version, or nil on error.
   `java -version` writes to stderr; some JVMs split between out and err."
  [java-bin]
  (try
    (let [{:keys [out err]} (proc/shell {:out :string :err :string :continue true}
                                        java-bin "-version")]
      (parse-major-version (str err out)))
    (catch Exception _ nil)))

(defn- system-jre-home
  "Return the home of a usable system JRE (Java `min-major-version`+), or nil.
   Checks $JAVA_HOME, then `java` on PATH."
  []
  (let [candidates (concat
                    (when-let [jh (System/getenv "JAVA_HOME")]
                      [{:home jh :bin (str (fs/path jh "bin" "java"))}])
                    (when-let [path-java (some-> (fs/which "java") str)]
                      ;; java's home is <prefix>/bin/java → <prefix>. Resolve
                      ;; the symlink so `~/.local/bin/java`-style shims point
                      ;; at the real install root.
                      [{:home (str (fs/parent (fs/parent (fs/canonicalize path-java))))
                        :bin  path-java}]))]
    (some (fn [{:keys [home bin]}]
            (when (fs/exists? bin)
              (when-let [v (system-java-version bin)]
                (when (>= v min-major-version) home))))
          candidates)))

(defn java-home
  "Return a usable JRE home: bundled if installed, otherwise a Java
   `min-major-version`+ runtime on the system. Nil if neither is available."
  []
  (or (when (installed?) paths/jre-dir)
      (system-jre-home)))

(defn- find-jre-root
  "After extracting, find the actual JRE root (may be nested in a directory).
   Filters out non-directory entries (e.g. the archive file)."
  [extract-dir]
  (let [dirs (filterv fs/directory? (fs/list-dir extract-dir))]
    (if (= 1 (count dirs))
      ;; macOS: jdk-21.../Contents/Home or jdk-21.../
      (let [inner (str (first dirs))
            home  (str (fs/path inner "Contents" "Home"))]
        (if (fs/exists? home) home inner))
      (str extract-dir))))

(defn- relocate!
  "Move src → target, falling back to recursive copy + delete if the
   underlying Files/move can't rename (typically a cross-filesystem move
   of a non-empty directory). WSL is the common case: /tmp lives on
   tmpfs while $HOME lives on ext4, so Files/move on the JRE's
   subdirectories throws a FileSystemException."
  [src target]
  (try
    (fs/move src target {:replace-existing true})
    (catch IOException _
      (fs/copy-tree src target {:replace-existing true})
      (fs/delete-tree src))))

(defn download!
  "Download and install JRE. Returns the JRE directory path."
  []
  (let [os       (detect-os)
        arch     (detect-arch)
        url      (adoptium-url os arch)
        s        (spinner/start (str "Downloading JRE " jre-version " for " os "/" arch "..."))
        s2-atom  (atom nil)
        ;; Stage under ~/.noumenon/ rather than the system temp dir so the
        ;; move into paths/jre-dir is always intra-filesystem. On WSL the
        ;; system /tmp is tmpfs and ~ is ext4, so a /tmp staging dir would
        ;; force a cross-fs move and break on the JRE's non-empty
        ;; subdirectories (`legal/`, `bin/`, `lib/`, ...).
        _        (fs/create-dirs paths/noum-dir)
        tmp-dir  (str (fs/create-temp-dir {:path paths/noum-dir :prefix "jre-staging-"}))
        ext      (if (= os "windows") ".zip" ".tar.gz")
        archive  (str (fs/path tmp-dir (str "jre" ext)))]
    (try
      (let [resp (http/get url {:as :stream :follow-redirects true})]
        (with-open [in (:body resp)
                    out (io/output-stream archive)]
          (io/copy in out)))
      ((:stop s) "JRE downloaded.")
      (verify-checksum! archive os arch)
      (let [s2 (spinner/start "Extracting JRE...")]
        (reset! s2-atom s2)
        (fs/create-dirs paths/jre-dir)
        (if (= ext ".zip")
          (fs/unzip archive tmp-dir)
          (proc/shell {:dir tmp-dir} "tar" "xzf" archive))
        ;; Move extracted contents to paths/jre-dir
        (let [root (find-jre-root tmp-dir)]
          (doseq [f (fs/list-dir root)]
            (let [target (str (fs/path paths/jre-dir (fs/file-name f)))]
              (when-not (str/ends-with? (str f) ext)
                (relocate! f target)))))
        ((:stop s2) "JRE installed."))
      paths/jre-dir
      (catch Exception e
        (if-let [s2 @s2-atom]
          ((:stop s2) "JRE extraction failed.")
          ((:stop s) "JRE download failed."))
        (throw e))
      (finally
        (fs/delete-tree tmp-dir)))))

(defn ensure!
  "Return a usable JRE home. Tries (1) the bundled JRE under ~/.noumenon/,
   (2) a system Java that satisfies `min-major-version`, (3) a fresh
   download as a last resort. Noumenon's uberjar targets Java
   `min-major-version`, so anything older is rejected here rather than
   blowing up later with UnsupportedClassVersionError."
  []
  (cond
    (installed?)
    paths/jre-dir

    :else
    (if-let [sys-home (system-jre-home)]
      (do (tui/eprintln (str "Using system Java at " sys-home
                             " (skipping " jre-version "+ bundled download)."))
          sys-home)
      (do (tui/eprintln (str "No Java " min-major-version "+ found. "
                             "First run: downloading JRE (~200MB) to ~/.noumenon/"))
          (download!)))))
