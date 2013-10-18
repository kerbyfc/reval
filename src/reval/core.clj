(ns reval.core
  (:import java.util.jar.JarFile)
  (:gen-class))

(defn list-jar
  "List files of inner-jar
  resources folder"
  [jar-path inner-dir]
  (if-let [jar (JarFile. jar-path)]
    (let [inner-dir (if (and (not= "" inner-dir) (not= "/" (last inner-dir))) (str inner-dir "/") inner-dir)
          entries      (enumeration-seq (.entries jar))
          names        (map (fn [x] (.getName x)) entries)
          snames       (filter (fn [x] (= 0 (.indexOf x inner-dir))) names)
          fsnames      (map #(subs % (count inner-dir)) snames)]
      fsnames)))

(defn has-file-extension?
  "Check if file
  extension is in
  one of the given"
  [file-name extensions]
  (let [extension-pattern (clojure.string/join "|" extensions)
        complete-pattern (str "^.+\\.(" extension-pattern ")$")
        extensions-reg-exp (re-pattern complete-pattern)]
    (if (re-find extensions-reg-exp file-name) true false)))

(defn filter-on-extensions
  "Filter filenames
  by it's extensions"
  [files extensions]
  (filter #(has-file-extension? % extensions) files))

(defn locate-jar
  "Utility function
  to get the name of jar
  in which this function
  is invoked"
  [& [ns]]
  (-> (class (or ns *ns*))
      .getProtectionDomain .getCodeSource .getLocation .getPath))

(defn list-jar-resources-dir
  "Get list of jar
  resource files by
  given path"
  [jar path]
  (list-jar jar path))

(defn get-local-resources-path
  "Get absolute resources
  path for given inner-resource
  folder"
  [path]
  (clojure.java.io/file (clojure.java.io/resource path)))

(defn list-local-resources-dir
  "Get list of local
  resource folder filnames"
  [path]
  (->> (file-seq (get-local-resources-path path))
       (filter #(.isFile %))
       (map #(.getName %))
       ))

(defn load-jar-resource [jar-path inner-path]
  "Get jar by path
  and read it inner-resource
  file contents"
  (if-let [jar   (JarFile. jar-path)]
    (if-let [entry (.getJarEntry jar inner-path)]
      (slurp (.getInputStream jar entry)))))

(defn file-path
  "Concatenate file path
  with OS-based file path
  separator"
  [& parts]
  (apply clojure.string/join java.io.File/separator parts))

(defn load-jar-resources
  "Initiate jar inner-resources
  files content reading and
  evaluation"
  [jar-path inner-path files]
  (doseq [file files]
    (eval (read-string (load-jar-resource jar-path (file-path [inner-path file]))))))

(defn load-local-resources
  "Initiate loading and
  evaluation of local
  inner-resource directory files"
  [path files]
  (doseq [file files]
    (load-file (file-path [(.getPath (get-local-resources-path path)) file]))))

(defmacro reval
  "Evaluates source code from files
  stored in separated inner-resource folder
  in concrete namespace after it's configuration
  Note: doesn't work with clojure namespace"
  [ns path & initer]
  `(do
     (if (not (= clojure.lang.Symbol (class ~ns)))
       (throw (Exception. (str "reval: first arg must be a clojure.lang.Symbol, " (class ~ns) " given"))))
     (try
       (def jar-path# (locate-jar ~ns))
       (catch Exception e# (println (.getMessage e# ))))
     (def jar# (and (bound? #'jar-path#)
                    (nil? (re-find #"clojure\-[\d]+" jar-path#))))
     (def filenames#
       (if jar#
         (list-jar-resources-dir jar-path# ~path)
         (list-local-resources-dir ~path)))
     (binding [*ns* *ns*]
       (in-ns ~ns)
       (refer-clojure)
       ~@initer
       (if jar#
         (load-jar-resources jar-path# ~path filenames#)
         (load-local-resources ~path filenames#)))))
