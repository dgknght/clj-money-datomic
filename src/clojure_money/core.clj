(ns clojure-money.core
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.defaults :refer :all]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [hiccup.core :refer :all]
            [clojure-money.web.accounts :as accounts]))

(defroutes app-routes
  (GET "/accounts" [] (accounts/index-accounts))
  (POST "/accounts" req (accounts/create-account (:params req)))
  (GET "/accounts/new" [] (accounts/new-account))
  (route/not-found (html [:h1 "Resource not found"])))

(def app (wrap-defaults app-routes site-defaults))

(defonce server (jetty/run-jetty #'app {:port 3000 :join? false}))

(defn -main
  "main entry point for the application"
  [& args]
  (.start server))
