(ns clojure-money.web.accounts
  (:require [clojure-money.accounts :as accounts]
            [clojure-money.common :as common]
            [clojure-money.web.layouts :refer :all]
            [datomic.api :as d]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]))


(defn accounts-layout
  "Layout for all account activities"
  [& content]
  (main-layout
    "Accounts"
    [:div {:class "page-header"}
     [:h1 "Accounts"]]
    content))

(defn index-accounts []
  (accounts-layout
    (let [conn (d/connect common/uri)]
      [:table
       [:tr
        [:th "Name"]]
       (doseq [account (accounts/all-accounts (d/db conn))]
         [:td (:name account)])])))

(defn new-account
  "Renders a form that can be submitted to create a new account"
  []
  (accounts-layout
    [:form {:role "form" :action "/accounts" :method "POST"}
     [:div {:class "form-group"}
      [:label {:for "name"} "Name"]
      [:text-field {:id "name" :name "name" :class "form-control"}]]
     [:button {:type "submit" :class "btn btn-default"} "Save"]]))
