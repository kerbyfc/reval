(ns reval.core
  (:import java.util.jar.JarFile)
  (:gen-class))

;; COMMON HELPERS

(defn has-file-extension?
  "Check if file extension is in one of the given.
  Takes filename and vector of extensions without dots."
  [file-name extensions]
  (let [extension-pattern (clojure.string/join "|" extensions)
        complete-pattern (str "^.+\\.(" extension-pattern ")$")
        extensions-reg-exp (re-pattern complete-pattern)]
    (if (re-find extensions-reg-exp file-name) true false)))

(defn filter-files-by-extensions
  "Filter filenames by it's extensions.
  Takes filename and vector of extensions without dots."
  [files extensions]
  (filter #(has-file-extension? % extensions) files))

(defn file-path
  "Concatenate file path
  with OS-based file path
  separator"
  [& parts]
  (apply clojure.string/join java.io.File/separator parts))

(defn ensure-ns
  [ns*]
  (if (symbol? ns*)
    (find-ns ns*)
    ns*))

(defn location
  [filepath]
  (clojure.string/replace filepath #"[\/]+[^\/]+$" ""))

(defn locate-user-root
  []
  (System/getProperty "user.home"))

;; JAR SECTION

(defn jar?
  [path]
  (re-find #"\.jar$" path))

(defn locate-jar
  "Utility function to get the name of jar in which this function is invoked."
  [ns*]
  (let [path (-> (class (ensure-ns ns*))
                 .getProtectionDomain
                 .getCodeSource
                 .getLocation
                 .getPath)
        same (re-find (re-pattern (clojure.string/replace (str (.getName (ensure-ns ns*))) #"\.\w+" "" )) path)]
    (if same
      path
      nil)
  ))

(defn get-jar
  [ns-or-path]
  (if (string? ns-or-path)
    (JarFile. ns-or-path)
    (if-let [jar-path (locate-jar ns-or-path)]
      (JarFile. jar-path))))

(defn list-jar-inner-resource-dir
  "List files of inner-jar
  resources folder"
  [jar-path inner-dir]
  (if-let [jar (get-jar jar-path)]
    (let [inner-dir (if (and (not= "" inner-dir) (not= "/" (last inner-dir))) (str inner-dir "/") inner-dir)
          entries      (enumeration-seq (.entries jar))
          names        (map (fn [x] (.getName x)) entries)
          snames       (filter (fn [x] (= 0 (.indexOf x inner-dir))) names)
          fsnames      (map #(subs % (count inner-dir)) snames)]
      fsnames)))

(defn read-jar-inner-resource-file
  "Get jar by path
  and read it inner-resource
  file contents"
  [jar-path inner-path]
  (if-let [jar (get-jar jar-path)]
    (if-let [entry (.getJarEntry jar inner-path)]
      (slurp (.getInputStream jar entry)))))

(defn load-jar-inner-resource-dir
  "Initiate jar inner-resources
  files content reading and
  evaluation"
  [jar-path inner-path]
  (doseq [file (list-jar-inner-resource-dir jar-path inner-path)]
    (eval (read-string (read-jar-inner-resource-file jar-path (file-path [inner-path file]))))))

;; LOCAL SECTION

(defn locate-application-root
  "Detect project path via class loader (clojure.java.io/resource)

    For project 'lol' classloader urls should be
    #<URL file:/.../lol/test/>
    #<URL file:/.../lol/src/>
    ...
    Project path should be #<URL file:/.../lol/>

  "
  ([]
   (location (.getPath (clojure.java.io/file (clojure.java.io/resource "")))))

  ([ns*]
   (if-let [jar-path (locate-jar ns*)]
     jar-path
     (locate-application-root))))

(defn inner-resource-path
  "Get absolute resources
  path for given inner-resource
  folder"
  [inner-path]
  (clojure.java.io/file (file-path [(str (locate-application-root *ns*) "/resources") inner-path])))

(defn list-inner-resource-dir
  "Get list of local
  resource folder filnames"
  [inner-path]
  (->> (file-seq (inner-resource-path inner-path))
       (filter #(.isFile %))
       (map #(.getName %))
       ))

(defn load-local-resources
  "Initiate loading and
  evaluation of local
  inner-resource directory files"
  [inner-path]
  (let [dir (inner-resource-path inner-path)]
    (doseq [file (list-inner-resource-dir inner-path)]
      (load-file (file-path [dir file])))))

(defmacro reval
  "Evaluates source code from files
  stored in separated inner-resource folder
  in concrete namespace after it's configuration
  Note: doesn't work with clojure namespace"
  [ns inner-path & initer]
  `(do
     (def root# (locate-application-root ~ns))
     (binding [*ns* *ns*]
       (in-ns ~ns)
       (refer-clojure)
       ~@initer
       (if (jar? root#)
         (load-jar-inner-resource-dir root# ~inner-path)
         (load-local-resources ~inner-path)))))

