(ns clj-money.core
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.defaults :refer :all]
            [compojure.core :refer :all]
            [clj-money.web.routes :as routes]))

(def app (wrap-defaults routes/app-routes site-defaults))

(defonce server (jetty/run-jetty #'app {:port 3204 :join? false}))

(defn -main
  "main entry point for the application"
  [& args]
  (.start server))
