(ns clojure-money.web.accounts
  (:require [clojure-money.accounts :as accounts]
            [clojure-money.common :as common]
            [clojure-money.web.layouts :refer :all]
            [datomic.api :as d]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]))


(defn index-accounts []
  (main-layout
    "Accounts"
    [:div {:class "page-header"}
     [:h1 "Accounts"]]
    (let [conn (d/connect common/uri)]
      [:table
       [:tr
        [:th "Name"]]
       (doseq [account (accounts/all-accounts (d/db conn))]
         [:td (:name account)])])))
