(defproject clj-money "0.1.0-SNAPSHOT"
  :description "Application implementing double-entry accounting"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/tools.logging "0.3.1" :exclusions [javax.mail/mail
                                                                 javax.jms/jms
                                                                 com.sun.jdmk/jmxtools
                                                                 com.sun.jmx/jmxri]]
                 [com.datomic/datomic-free "0.9.5067" :exclusions [joda-time
                                                                   org.slf4j/slf4j-nop
                                                                   org.slf4j/slj4j-log4j12]]
                 [ch.qos.logback/logback-classic "1.1.3"]
                 [clj-time "0.9.0"]
                 [ring "1.4.0"]
                 [ring/ring-defaults "0.1.5"]
                 [compojure "1.4.0"]
                 [hiccup "1.0.5"]]
  :main ^:skip-aot clj-money.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :plugins [[cider/cider-nrepl "0.9.1"]])
