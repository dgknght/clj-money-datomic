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

(defn transaction-row
  [{transaction-date :transaction/transaction-date description :transacdtion/description :as transaction}]
  [:tr
   [:td transaction-date]
   [:td description]])

(defn index-transactions
  []
  (main-layout
    "Transactions"
    [:div.page-header
     [:h1 "Transactions"]]
    [:table.table.table-striped
     [:tr
      [:th "Date"]
      [:th "Description"]
      [:th "Amount"]]
     (let [conn (d/connect common/uri)]
      (->> (transactions/get-transactions (d/db conn) (-> 7 t/days t/ago))
          (map transaction-row)))]
    [:a.btn.btn-default {:href "/transactions/new"} "New"]))

(defn transaction-item-row
  [index]
  [:tr
   [:td
    [:input.form-control {:type "text" :name (str "transaction-item-account-" index) :id (str "transaction-item-account-" index)}]]
   [:td
    [:input.form-control {:type "number" :step 0.01 :name (str "transaction-item-debit-" index) :id (str "transaction-item-debit-" index)}]]
   [:td
    [:input.form-control {:type "number" :step 0.01 :name (str "transaction-item-credit-" index) :id (str "transaction-item-credit-" index)}]]])

(defn form-fields
  []
  (html
    (anti-forgery-field)
    [:div.form-group
     [:label {:for "transaction-date"} "Transaction Date"]
     [:input.form-control.date-field {:type "text" :name "transaction-date" :id "transaction-date" :autofocus true}]]
    [:div.form-group
     [:label {:for "description"} "Description"]
     [:input.form-control {:type "text" :name "description" :id "description"}]]
    [:table.table.table-striped
     [:tr
      [:th.col-md-6 "Account"]
      [:th.col-md-3 "Debit"]
      [:th.col-md-3 "Credit"]]
     (map transaction-item-row (range 9))]
    [:div.btn-group
     [:input.btn.btn-primary {:type "submit" :value "Save"}]
     [:a.btn.btn-default {:href "/transactions"} "Cancel"]]))

(defn new-transaction
  []
  (main-layout
    "New Transaction"
    [:div.page-header
     [:h1 "New Transaction"]]
    [:form {:action "/transactions" :method "POST"}
     (form-fields)]))

(defn extract-transaction-item
  [params index]
  (let [[account debit credit] (->> ["account" "debit" "credit"]
                                    (map #(keyword (str "transaction-item-" % "-" index)))
                                    (map #(% params)))]
    (when-not (empty? account)
      {:transaction-item/account account
       :transaction-item/action (if (empty? debit) :transaction-item.action/credit :transaction-item.action/debit)
       :transaction-item/amount (if (empty? debit) (bigdec credit) (bigdec debit))})))

(defn extract-transaction-items
  [params]
  (->> (iterate inc 0)
       (map #(extract-transaction-item params %))
       (take-while identity)))

(defn transaction-params
  [params]
  (-> params
      (select-keys [:transaction-date :description])
      (rename-keys {:transaction-date :transaction/date
                    :description :transaction/description})
      (update :transaction/date util/parse-date)
      (assoc :transaction/items (extract-transaction-items params))))

(defn create-transaction
  [params]

  (log/debug "dbk create-transaction " (prn-str (transaction-params params)))

  (let [conn (d/connect common/uri)]
    (transactions/add-transaction conn (transaction-params params)))
  (redirect "/transactions"))