(ns clj-money.core
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.defaults :refer :all]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [hiccup.core :refer :all]
            [clj-money.web.accounts :as accounts]
            [clj-money.web.transactions :as transactions]
            [clj-money.web.pages :as pages]))

(defroutes app-routes
  (GET "/" [] (pages/home))

  (GET "/accounts" [] (accounts/index-accounts))
  (POST "/accounts" [:as {params :params}] (accounts/create-account params))
  (GET "/accounts/new" [] (accounts/new-account))
  (GET "/accounts/:id" [id] (accounts/show-account id))
  (GET "/accounts/:id/edit" [id] (accounts/edit-account id))
  (POST "/accounts/:id" [id :as {params :params}] (accounts/update-account id params))
  (POST "/accounts/:id/delete" [id] (accounts/delete-account id))

  (GET "/transactions" [] (transactions/index-transactions))
  (GET "/transactions/new" [] (transactions/new-transaction))
  (POST "/transactions" [:as {:keys [params]}] (transactions/create-transaction params))
  (GET "/transactions/:id" [id] (transactions/show-transaction id))
  (GET "/transactions/:id/edit" [id] (transactions/edit-transaction id))
  (POST "/transactions/:id" [id :as {params :params}] (transactions/update-transaction id params))

  (route/not-found (html [:h1 "Resource not found"])))

(def app (wrap-defaults app-routes site-defaults))

(defonce server (jetty/run-jetty #'app {:port 3204 :join? false}))

(defn -main
  "main entry point for the application"
  [& args]
  (.start server))
