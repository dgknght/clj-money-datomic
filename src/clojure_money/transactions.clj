(ns clojure-money.transactions
  (:require [datomic.api :as d :refer :all]
            [clojure-money.core :refer :all]
            [clojure-money.accounts :refer :all])
  (:gen-class))

;; ----- Primary methods -----

(declare resolve-transactions-enums)
(defn get-transactions
  "Returns the transactions in the specified timeframe"
  [conn start-date end-date]
  (let [db (d/db conn)]
    (->> (d/q
           '[:find ?t
             :in $ ?start-date ?end-date
             :where [?t :transaction/date ?transaction-date]
             [(> ?transaction-date ?start-date)]
             [(< ?transaction-date ?end-date)]]
           db
           start-date
           end-date)
         (map first)
         (pull-many db '[*])
         (resolve-transactions-enums conn))))

(declare transaction-items->tx-data)
(declare valid-transaction-data?)
(defn add-transaction
  "Adds a new transaction to the system"
  [conn transaction-date description items]
  (if-not (valid-transaction-data? transaction-date description items)
    (throw (IllegalArgumentException. "The transaction data is not valid."))) ; TODO add details about the invalid data
  (let [new-id (d/tempid :db.part/user)
        items-tx-data (transaction-items->tx-data conn items)]
    @(d/transact
       conn
       [{:db/id new-id
         :transaction/date transaction-date
         :transaction/description description
         :transaction/items items-tx-data}])))

;; ----- Helper methods -----

(defn transaction-items->tx-data
  "Converts the raw transaction item input data into something suitable for a transact invocation"
  [conn items]
  (apply vector (map
       (fn [[action account-name amount]] {:db/id (d/tempid :db.part/user)
          :transaction-item/account (find-account-id-by-path conn account-name)
          :transaction-item/action action
          :transaction-item/amount amount})
       items)))

(defn resolve-action
  "Looks up a transaction item action from a db/id"
  [db action]
  (get-ident db (:db/id action)))

(defn resolve-transaction-item-enums
  "Looks up references in a list of transaction item maps"
  [db item]
  (assoc item
         :transaction-item/action (resolve-action db (:transaction-item/action item))))

(defn resolve-transaction-items-enums
  "Looks up references in a list of transaction items"
  [db items]
  (map #(resolve-transaction-item-enums db %) items))

(defn resolve-transaction-enums
  "Looks up references in transaction map"
  [db transaction]
  (assoc transaction
         :transaction/items (resolve-transaction-items-enums
                              db
                              (:transaction/items transaction))))

(defn resolve-transactions-enums
  "Looks up references in a list of transaction maps"
  [conn transactions]
  (let [db (d/db conn)]
    (map #(resolve-transaction-enums db %) transactions)))

(defn item-total
  "returns the total of the credit actions"
  [action items]
  (->> items
       (clojure.core/filter #(= action (first %)))
       (map last)
       (reduce +)))

(defn credit-debit-balanced?
  "Returns a boolean value indicating whether or not the credit and debit totals are equal in the specified items"
  [items]
  (let [credit-total (item-total :transaction-item.action/credit items)
        debit-total (item-total :transaction-item.action/debit items)]
    (= credit-total debit-total)))

(defn valid-transaction-data?
  "Returns a boolean value indicating whether or not the transaction data is valid"
  [transaction-date description items]
  (cond
    (nil? transaction-date) false
    (nil? description) false
    (not (credit-debit-balanced? items)) false
    :else true))
