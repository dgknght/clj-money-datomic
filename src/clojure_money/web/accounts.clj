(ns clojure-money.web.accounts
  (:require [clojure-money.accounts :as accounts]
            [clojure-money.common :as common]
            [clojure-money.web.layouts :refer :all]
            [datomic.api :as d]
            [ring.util.anti-forgery :refer :all]
            [ring.util.response :refer [redirect]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]))

(defn index-accounts []
  (main-layout
    "Accounts"
    [:div.page-header
     [:h1 "Accounts"]]
    [:table.table
     [:tr
      [:th "Name"]]
     (let [conn (d/connect common/uri)
           db (d/db conn)
           list (accounts/all-accounts db)]
       (map (fn [{account-name :account/name}]
              [:tr
               [:td account-name]])
            list))]
    [:a.btn.btn-default {:href "accounts/new"} "New"]))

(defn account-options-for-select
  "Returns the HTML options for the available accounts"
  []
  (let [conn (d/connect common/uri)]
    (map #(vector :option {:value (:id %)} (:name %))
         (accounts/all-accounts (d/db conn)))))

(defn new-account
  "Renders a form that can be submitted to create a new account"
  []
  (main-layout
    "New Account"
    [:div.page-header
     [:h1 "New Account"]]
    [:form {:role "form" :action "/accounts" :method "POST"}
     (anti-forgery-field)
     [:div.form-group
      [:label {:for "account-type"} "Account Type"]
      [:select.form-control {:id "account-type" :name "account-type" :autofocus 1}
       [:option "asset"]
       [:option "liability"]
       [:option "equity"]
       [:option "income"]
       [:option "expense"]]]
     [:div.form-group
      [:label {:for "name"} "Name"]
      [:input.form-control {:id "name" :name "name" :type "text"}]]
     [:div.form-group
      [:label {:for "parent"} "Parent"]
      [:select.form-control {:id "parent" :name "parent"}
       [:option {:value ""} "--none--"]
       (account-options-for-select)]]
     [:button.btn.btn-default {:type "submit"} "Save"]]))

(defn create-account
  "Creates an account using the supplied parameters, redirecting to the account list on success, or the account form on failure"
  [{:keys [name account-type parent]}]
  (let [conn (d/connect common/uri)]
    (accounts/add-account conn name (symbol (str "account.type/" account-type)) parent))
  (redirect "/accounts"))
