(defproject reval "0.1.2-SNAPSHOT"
  :description "Resource files executor for clojure applications"
  :url "https://github.com/kerbyfc/reval"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :main reval.core
  :profiles {:uberjar {:aot :all}})
