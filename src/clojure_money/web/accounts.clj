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
    [:div.page-header
     [:h1 "Accounts"]]
    (let [conn (d/connect common/uri)]
      [:table.table
       [:tr
        [:th "Name"]]
       (doseq [account (accounts/all-accounts (d/db conn))]
         [:td (:name account)])])
    [:a.btn.btn-default {:href "accounts/new"} "New"]))

(defn new-account
  "Renders a form that can be submitted to create a new account"
  []
  (main-layout
    "New Account"
    [:div.page-header
     [:h1 "New Account"]]
    [:form {:role "form" :action "/accounts" :method "POST"}
     [:div.form-group
      [:label {:for "name"} "Name"]
      [:text-field.form-control {:id "name" :name "name" :autofocus 1}]]
     [:div.form-group
      [:label {:for "parent"}]
      [:select.form-control
       [:option {:value 1} "Other account"]
       [:option {:value 2} "Different account"]]]
     [:button.btn.btn-default {:type "submit"} "Save"]]))
