(ns clojure-money.web.accounts
  (:require [clojure-money.accounts :as accounts]
            [clojure-money.web.layouts :refer :all]
            [datomic.api :as d]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]))

; TODO read this from the settings
(def uri "datomic:free://localhost:4334/money")

(defn index-accounts []
  (main-layout
    "Accounts"
    [:div {:class "page-header"}
     [:h1 "Accounts"]]
    (let [conn (d/connect uri)]
      [:table
       [:tr
        [:th "Name"]]
       (doseq [account (accounts/all-accounts (d/db conn))]
         [:td (:name account)])])))
