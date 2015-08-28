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
      [:span.glyphicon.glyphicon-remove {:aria-hidden true}]]])

(defn account-row
  [{account-name :account/name id :db/id :as account}]
  [:tr
   [:td account-name]
   [:td
    [:div.pull-left
    [:a.btn.btn-link.btn-sm {:href (str "/accounts/" id "/edit")}
     [:span.glyphicon.glyphicon-pencil {:aria-hidden true}]]]
    (delete-form "accounts" id)]])

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

(defn select-option
  [caption value selected]
  (let [option [:option {:value (or value caption)} caption]]
    (if selected
      (assoc-in option [1 :selected] true)
      option)))

(defn account-options-for-select
  "Returns the HTML options for the available accounts"
  [selected-id]
  (let [conn (d/connect common/uri)]
    (map (fn [{id :db/id account-name :account/name}]
           (select-option account-name id (= selected-id id)))
         (accounts/all-accounts (d/db conn)))))

(defn account-type-options
  [selected-type]
  (map (fn [account-type]
         (select-option account-type account-type (= selected-type account-type)))
       ["asset" "liability" "equity" "income" "expense"]))

(defn form-fields
  ([] (form-fields {}))
  ([account]
   [:div ; TODO This div is only here because this function returns a collection and that's what hiccup needs
    (anti-forgery-field)
    [:div.form-group
     [:label {:for "account-type"} "Account Type"]
     [:select.form-control {:id "account-type" :name "account-type" :autofocus 1}
      (account-type-options (:account/type account))]]
    [:div.form-group
     [:label {:for "name"} "Name"]
     [:input.form-control {:id "name" :name "name" :type "text" :value (:account/name account)}]]
    [:div.form-group
     [:label {:for "parent-id"} "Parent"]
     [:select.form-control {:id "parent-id" :name "parent-id"}
      [:option {:value ""} "--none--"]
      (account-options-for-select (:account/parent account))]]]))

(defn new-account
  "Renders a form that can be submitted to create a new account"
  []
  (main-layout
    "New Account"
    [:div.page-header
     [:h1 "New Account"]]
    [:form {:role "form" :action "/accounts" :method "POST"}
     (form-fields)
     [:div.btn-group
      [:button.btn.btn-primary {:type "submit"} "Save"]
      [:a.btn.btn-default {:href "/accounts"} "Cancel"]]]))

(defn edit-account
  "Renders a form that can be submitted to modify an existing account"
  [account-id]
  (let [conn (d/connect common/uri)
        account (accounts/find-account (d/db conn) (Long. account-id))]
    (main-layout
      "Edit Account"
      [:div.page-header
       [:h1 "Edit Account"]]
      [:form {:role "form" :action (str "/accounts/" account-id) :method "POST"}
       (form-fields account)
       [:div.btn-group
        [:button.btn.btn-primary {:type "submit"} "Save"]
        [:a.btn.btn-default {:href "/accounts"} "Cancel"]]])))

(defn account-params
  [params]
  (-> params
      (select-keys [:account-type :parent-id :name])
      (clojure.set/rename-keys {:name :account/name
                                :account-type :account/type
                                :parent-id :account/parent})
      (update-in [:account/parent] #(Long. %))
      (update-in [:account/type] #(keyword (str "account.type/" %)))))

(defn create-account
  "Creates an account using the supplied parameters, redirecting to the account list on success, or the account form on failure"
  [params]
  (let [{:keys [name parent-id account-type]} (account-params params)
        conn (d/connect common/uri)]
    (accounts/add-account conn name (symbol (str "account.type/" account-type)) parent-id))
  (redirect "/accounts")) ; TODO Need to handle error messages

(defn update-account
  "Updates an existing account"
  [account-id params]
  (let [id (Long. account-id)
        account-params (account-params params)
        conn (d/connect common/uri)]
    (accounts/update-account conn id account-params)
    (redirect "/accounts"))) ; TODO Need to include error messages

(defn delete-account
  "Deletes the specified account"
  [account-id]
  (let [conn (d/connect common/uri)]
    (accounts/delete-account conn (Long. account-id)))
  (redirect "/accounts"))
