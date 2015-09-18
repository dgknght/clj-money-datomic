(ns clj-money.web.transactions
  (:require [clojure.tools.logging :as log]
            [clojure.set :refer [rename-keys]]
            [clj-money.transactions :as transactions]
            [clj-money.accounts :as accounts]
            [clj-money.common :as common]
            [clj-money.web.layouts :refer :all]
            [clj-money.util :as util]
            [datomic.api :as d]
            [ring.util.anti-forgery :refer :all]
            [ring.util.response :refer [redirect]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [clj-time.core :as t]
            [clj-time.coerce :as c]))

(defn extract-transaction-item
  [params index]
  (let [[account debit credit id] (->> ["account" "debit" "credit" "id"]
                                       (map #(keyword (str "transaction-item-" % "-" index)))
                                       (map #(% params)))]
    (when-not (or (empty? account) (and (empty? debit) (empty? credit)))
      (let [required {:transaction-item/action (if (empty? debit) :transaction-item.action/credit :transaction-item.action/debit)
                      :transaction-item/amount (if (empty? debit) (bigdec credit) (bigdec debit))
                      :transaction-item/account account}]
        (if (empty? id)
          required
          (assoc required :db/id (util/parse-long id)))))))

(defn extract-transaction-items
  [params]
  (->> (iterate inc 0)
       (map #(extract-transaction-item params %))
       (take-while identity)))

(defn transaction-params
  [{id :id :as params}]
  (let [required (-> params
                     (select-keys [:transaction-date :description])
                     (rename-keys {:transaction-date :transaction/date
                                   :description      :transaction/description})
                     (update :transaction/date util/parse-date)
                     (assoc :transaction/items (extract-transaction-items params)))]
    (if (empty? id)
      required
      (assoc required :db/id (util/parse-long id)))))

(defn update-transaction
  [id params]
  (let [conn (d/connect common/uri)]
    (transactions/update-transaction conn (transaction-params params)))
  (redirect "/transactions"))

(defn transaction-item-form-row
  [index {amount :transaction-item/amount
          action :transaction-item/action
          account :transaction-item/account
          :as item}]
  (let [credit-amount (if (= :transaction-item.action/credit action) amount nil)
        debit-amount (if (= :transaction-item.action/debit action) amount nil)
        account-field (str "transaction-item-account-" index)
        account-name (if account (-> account d/touch :account/name) nil)
        debit-amount-name (str "transaction-item-debit-" index)
        credit-amount-name (str "transaction-item-credit-" index)
        id-name (str "transaction-item-id-" index)]
    [:tr
     [:td
      [:input {:type "hidden" :name id-name :value (:db/id item)}]
      [:input.form-control {:type "text"
                            :name account-field
                            :id account-field
                            :value account-name}]] ; TODO Need to get the full path
     [:td
      [:input.form-control {:type "number"
                            :step 0.01
                            :name debit-amount-name
                            :id debit-amount-name
                            :value debit-amount}]]
     [:td
      [:input.form-control {:type "number"
                            :step 0.01
                            :name credit-amount-name
                            :id credit-amount-name
                            :value credit-amount}]]]))

(defn form-fields
  ([] (form-fields {}))
  ([transaction]
   (html
     (anti-forgery-field)
     [:div.row
      [:div.col-md-6
       [:div.form-group
        [:label {:for "transaction-date"} "Transaction Date"]
        [:input.form-control.date-field {:type "text" :name "transaction-date" :id "transaction-date" :value (util/format-date (:transaction/date transaction)) :autofocus true}]]
       [:div.form-group
        [:label {:for "description"} "Description"]
        [:input.form-control {:type "text" :name "description" :id "description" :value (:transaction/description transaction)}]]]]
     [:table.table.table-striped
      [:tr
       [:th.col-md-6 "Account"]
       [:th.col-md-3 "Debit"]
       [:th.col-md-3 "Credit"]]
      (map-indexed transaction-item-form-row (take 10 (concat (:transaction/items transaction)
                                                              (repeat {}))))]
     [:div.btn-group
      [:input.btn.btn-primary {:type "submit" :value "Save" :title "Click here to save the transaction."}]
      [:a.btn.btn-default {:href "/transactions" :title "Click here to return to the list of transactions."} "Cancel"]])))

(defn edit-transaction
  [id]
  (main-layout
    "Edit Transaction"
    [:div.page-header
     [:h1 "Edit Transaction"]]
    (let [conn (d/connect common/uri)
          transaction (transactions/get-transaction (d/db conn) (java.lang.Long/parseLong id))]
      [:form {:action (str "/transactions/" id) :method "POST"}
       (form-fields transaction)])))

(defn transaction-item-row
  [{account :transaction-item/account action :transaction-item/action amount :transaction-item/amount}]
  [:tr
   [:td (-> account
            d/touch
            :account/name)]
   [:td action]
   [:td (util/format-number amount)]])

(defn show-transaction
  [id]
  (main-layout
    "Transaction"
    [:div.page-header
     [:h1 "Transaction"]]
    (let [conn (d/connect common/uri)
         transaction (transactions/get-transaction (d/db conn) (java.lang.Long/parseLong id))]
      (html [:div.row
             [:div.col-md-4
              [:table.table.table-striped
               [:tr
                [:th.col-md-2 "Date"]
                [:td.col-md-4 (util/format-date (:transaction/date transaction))]]
               [:tr
                [:th "Description"]
                [:td (:transaction/description transaction)]]]]]
            [:table.table.table-striped
             [:tr
              [:th "Account"]
              [:th "Action"]
              [:th "Amount"]]
             (map transaction-item-row (:transaction/items transaction))]))
    [:a.btn.btn-default {:href "/transactions"} "Back"]))

(defn transaction-row
  [{transaction-date :transaction/date
    description :transaction/description
    items :transaction/items
    id :db/id
    :as transaction}]
  (let [amount (->> items
                    (filter #(= :transaction-item.action/credit (:transaction-item/action %)))
                    (map :transaction-item/amount)
                    (reduce #(+ %1 %2) 0))]
  [:tr
   [:td (util/format-date transaction-date)]
   [:td description]
   [:td.text-right [:a {:href (str "/transactions/" id)} (util/format-number amount)]]
   [:td
    [:div.pull-left
     [:a.btn.btn-link.btn-sm {:href (str "/transactions/" id "/edit") :title "Click here to edit this transaction."}
      [:span.glyphicon.glyphicon-pencil {:area-hidden true}]]]
    (delete-form "transactions" id)]]))

(defn index-transactions
  []
  (main-layout
    "Transactions"
    [:div.page-header
     [:h1 "Transactions"]]
    [:table.table.table-striped
     [:tr
      [:th.col-md-1 "Date"]
      [:th.col-md-9 "Description"]
      [:th.col-md-1.text-right "Amount"]
      [:td.col-md-1 "&nbsp;"]]
     (let [conn (d/connect common/uri)]
       (->> (transactions/get-transactions (d/db conn))
            (map transaction-row)))]
    [:a.btn.btn-default {:href "/transactions/new"} "New"]))

(defn new-transaction
  []
  (main-layout
    "New Transaction"
    [:div.page-header
     [:h1 "New Transaction"]]
    [:form {:action "/transactions" :method "POST"}
     (form-fields)]))

(defn create-transaction
  [params]

  (log/debug "dbk create-transaction " (prn-str (transaction-params params)))

  (let [conn (d/connect common/uri)]
    (transactions/add-transaction conn (transaction-params params)))
  (redirect "/transactions"))
