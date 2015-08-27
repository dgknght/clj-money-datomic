(ns clojure-money.web.accounts
  (:require [clojure-money.accounts :as accounts]
            [clojure-money.common :as common]
            [clojure-money.web.layouts :refer :all]
            [datomic.api :as d]
            [ring.util.anti-forgery :refer :all]
            [ring.util.response :refer [redirect]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]))

(defn delete-form
  [model-type id]
  [:form.form-inline {:action (str "/" model-type "/" id "/delete")
                        :method "POST"
                        :style "margin: 0; padding: 0;"}
     (anti-forgery-field)
     [:button.btn.btn-sm.btn-link {:type "submit" :title "Click here to delete the account."}
      [:span.glyphicon.glyphicon-remove {:area-hidden true}]]])

(defn account-row
  [{account-name :account/name id :db/id :as account}]
  [:tr
   [:td account-name]
   [:td (delete-form "accounts" id)]])

(defn index-accounts []
  (main-layout
    "Accounts"
    [:div.page-header
     [:h1 "Accounts"]]
    [:table.table
     [:tr
      [:th "Name"]
      [:td "&nbsp;"]]
     (let [conn (d/connect common/uri)
           db (d/db conn)
           list (accounts/all-accounts db)]
       (map account-row list))]
    [:a.btn.btn-default {:href "accounts/new"} "New"]))

(defn account-options-for-select
  "Returns the HTML options for the available accounts"
  []
  (let [conn (d/connect common/uri)]
    (map (fn [{id :db/id account-name :account/name}]
           [:option {:value id} account-name])
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
      [:label {:for "parent-id"} "Parent"]
      [:select.form-control {:id "parent-id" :name "parent-id"}
       [:option {:value ""} "--none--"]
       (account-options-for-select)]]
     [:button.btn.btn-default {:type "submit"} "Save"]]))

(defn create-account
  "Creates an account using the supplied parameters, redirecting to the account list on success, or the account form on failure"
  [{:keys [name account-type parent-id]}]
  (let [conn (d/connect common/uri)]
    (accounts/add-account conn name (symbol (str "account.type/" account-type)) parent-id))
  (redirect "/accounts"))

(defn delete-account
  "Deletes the specified account"
  [account-id]
  (let [conn (d/connect common/uri)]
    (accounts/delete-account conn (Long. account-id)))
  (redirect "/accounts"))
