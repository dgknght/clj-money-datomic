(ns clj-money.web.accounts
  (:require [clojure.tools.logging :as log]
            [clojure.set :refer [rename-keys]]
            [clj-money.accounts :as accounts]
            [clj-money.transactions :as transactions]
            [clj-money.reports :as reports]
            [clj-money.common :as common]
            [clj-money.util :as util]
            [clj-money.web.layouts :refer :all]
            [datomic.api :as d]
            [ring.util.anti-forgery :refer :all]
            [ring.util.response :refer [redirect]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]))

(defn account-transaction-row
  [[item transaction]]
  [:tr
   [:td (util/format-date (:transaction/date transaction))]
   [:td (:transaction/description transaction)]
   [:td (util/format-number (:transaction-item/amount item))]
   [:td "&nbsp;"]])

(defn account-transactions-table
  [db account]
  [:table.table.table-striped.table-hover
   [:th "Date"]
   [:th "Description"]
   [:th "Amount"]
   [:th "Balance"]
   (map account-transaction-row (transactions/get-account-transaction-items db (:db/id account)))])

(defn show-account
  [id]
  (main-layout
    "Account"
    (let [db (d/db (d/connect common/uri))
          account (accounts/find-account db (java.lang.Long/parseLong id))]
      (html [:div.page-header
             [:h1 (:account/name account)]]
            (account-transactions-table db account)
            [:div.btn-group
             [:a.btn.btn-default {:href "/accounts"} "Back"]]))))

(defn delete-form
  [model-type id]
  [:form.form-inline {:action (str "/" model-type "/" id "/delete")
                        :method "POST"
                        :style "margin: 0; padding: 0;"}
     (anti-forgery-field)
     [:button.btn.btn-sm.btn-link {:type "submit" :title "Click here to delete the account."}
      [:span.glyphicon.glyphicon-remove {:aria-hidden true}]]])

(defn account-row
  [{:keys [style caption depth account-type] {id :db/id} :account :as display-record}]
  [:tr
   (if (= :header style)
     [:th caption]
     [:td
      [:div {:class (str "depth-" depth)}
       [:a {:href (str "/accounts/" id)} caption]]])
   [:td
    [:div.pull-left
     [:a.btn.btn-link.btn-sm {:href (str "/accounts/" id "/edit")}
      [:span.glyphicon.glyphicon-pencil {:aria-hidden true}]]]
    (delete-form "accounts" id)]])

(defn account-to-displayable
  [account]
  (rename-keys (into {} account) {:account/name :caption :db/id :id :account/type :account-type}))

(defn index-accounts []
  (main-layout
    "Accounts"
    [:div.page-header
     [:h1 "Accounts"]]
    [:table.table.table-striped
     (let [conn (d/connect common/uri)
           list (reports/account-list-with-headers (d/db conn))]
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
  [selected-id {:keys [except]}]
  (let [conn (d/connect common/uri)]
    (let [result (->> (reports/display-records (d/db conn))
                      (remove #(except (-> % :account :db/id)))
                      (group-by :account-type)
                      (reduce (fn [output [account-type display-records]]
                                (let [result (-> [:optgroup {:label (account-type reports/account-type-caption-map)}]
                                                 (concat (mapv (fn [{{id :db/id} :account path :path}]
                                                                 (select-option path id (= selected-id id)))
                                                               display-records))
                                                 vec)]
                                  (concat output [result])))
                              []))]
      (when (seq result)
        result))))

(defn account-type-options
  [selected-type]
  (map (fn [account-type]
         (select-option account-type account-type (= selected-type account-type)))
       ["asset" "liability" "equity" "income" "expense"]))

(defn form-fields
  ([] (form-fields {}))
  ([{id :db/id account-type :account/type account-name :account/name {parent-id :db/id} :account/parent}]
   [:div ; TODO This div is only here because this function returns a collection and that's what hiccup needs
    (anti-forgery-field)
    [:div.form-group
     [:label {:for "account-type"} "Account Type"]
     [:select.form-control {:id "account-type" :name "account-type" :autofocus 1}
      (account-type-options (when account-type (name account-type)))]]
    [:div.form-group
     [:label {:for "name"} "Name"]
     [:input.form-control {:id "name" :name "name" :type "text" :value account-name}]]
    [:div.form-group
     [:label {:for "parent-id"} "Parent"]
     [:select.form-control {:id "parent-id" :name "parent-id"}
      [:option {:value ""} "--none--"]
      (account-options-for-select parent-id {:except #{id}})]]]))

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
  [{:keys [account-type parent-id name]}]
  (let [result {:account/name name
                :account/type (keyword (str "account.type/" account-type))}]
    (if (empty? parent-id)
      result
      (assoc result :account/parent (Long/parseLong parent-id)))))

(defn create-account
  "Creates an account using the supplied parameters, redirecting to the account list on success, or the account form on failure"
  [params]
  (try
    (let [conn (d/connect common/uri)]
      (accounts/add-account conn (account-params params))
      (redirect "/accounts"))
    (catch Exception e
      (log/error e "Unable to create the new account.")
      (html [:pre (prn-str params)]
            [:pre (prn-str e)])))) ; TODO Need a universal way to handle error messages

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
