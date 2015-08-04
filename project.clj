(defproject clojure-money "0.1.0-SNAPSHOT"
  :description "Application implementing double-entry accounting"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [com.datomic/datomic-free "0.9.5067" :exclusions [joda-time]]
                 [clj-time "0.9.0"]
                 [ring "1.4.0"]
                 [compojure "1.4.0"]]
  :main ^:skip-aot clojure-money.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :plugins [[lein-expectations "0.0.7"]])
