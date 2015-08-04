(ns clojure-money.core
  (:require [ring.adapter.jetty :as jetty]
            [compojure.core :refer :all]
            [compojure.route :as route]))

(defroutes app
  (GET "/" [] "<html><body><h1>Hello!</h1>It's working!</body></html>")
  (GET "/test" [] "<html><body><h1>Testing!</h1>One, Two, Three</body></html>")
  (route/not-found "<h1>Resource not found</h1>"))

(defn -main
  "main entry point for the application"
  [& args]
  (jetty/run-jetty app {:port 3000}))
