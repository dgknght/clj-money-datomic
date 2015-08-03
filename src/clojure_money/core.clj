(ns clojure-money.core
  (:require [ring.adapter.jetty :as jetty]))

(defn handler [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "<html><body><h1>Hello!</h1>It's working!</body></html>"})

(defn -main
  "main entry point for the application"
  [& args]
  (jetty/run-jetty handler {:port 3000}))
