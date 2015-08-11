(ns clojure-money.core
  (:require [ring.adapter.jetty :as jetty]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [hiccup.core :refer :all]
            [clojure-money.web.accounts :as accounts]))

(defroutes app
  (GET "/accounts" [] (accounts/index-accounts))
  (route/not-found (html [:h1 "Resource not found"])))

(defn -main
  "main entry point for the application"
  [& args]
  (jetty/run-jetty app {:port 3000}))

(defn go! []
  (jetty/run-jetty app {:port 3000}))
