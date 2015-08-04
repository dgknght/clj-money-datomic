(ns clojure-money.core
  (:require [ring.adapter.jetty :as jetty]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [hiccup.core :refer :all]))

(defroutes app
  (GET "/" [] (html [:html [:body [:h1 "Hello!"] "It's working!"]]))
  (GET "/test" [] (html [:html [:body [:h1 "Testing!"] "1, 2, 3"]]))
  (route/not-found "<h1>Resource not found</h1>"))

(defn -main
  "main entry point for the application"
  [& args]
  (jetty/run-jetty app {:port 3000}))
