(ns clojure-money.transactions
  (:require [datomic.api :as d :refer [tempid q db transact pull-many]]
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

(declare resolve-transaction-data)
(declare valid-transaction-data?)
(declare create-balance-adjustment-tx-data)
(defn add-transaction
  "Adds a new transaction to the system"
  [conn data]
  (when-not (valid-transaction-data? data)
    (throw (IllegalArgumentException. "The transaction data is not valid."))) ; TODO add details about the invalid data
  (let [new-id (d/tempid :db.part/user)
        complete-data (->> data
                           (resolve-transaction-data conn)
                           (merge {:db/id new-id}))
        all-tx-data (concat
                      [complete-data]
                      (create-balance-adjustment-tx-data conn (:transaction/items complete-data)))]
    @(d/transact conn all-tx-data)))

;; ----- Helper methods -----

(defn create-balance-adjustment-tx-data
  "Adds tx-data for adjusting account balances for a transaction"
  [conn items]
  (map
    (fn [{account :transaction-item/account action :transaction-item/action amount :transaction-item/amount}]
      [:adjust-balance conn account amount])
    items))

(defn resolve-account
  "Resolves the information into an account id. The input may be a path, account entity, or id"
  [conn token]
  (cond (string? token) (find-account-by-path conn token)
        (integer? token) (d/entity (d/db conn) token)
        :else token))

(defn resolve-transaction-item-data
  "Resolves references inside transaction item data"
  [conn data]
  (assoc-in data [:transaction-item/account] (:db/id (resolve-account conn (:transaction-item/account data)))))

(defn resolve-transaction-data
  "Resolves references inside transaction data"
  [conn data]
  (assoc-in data [:transaction/items] (map #(resolve-transaction-item-data conn %) (:transaction/items data))))

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
  "returns the total of the debit or credit actions"
  [action items]
  (->> items
       (clojure.core/filter #(= action (:transaction-item/action %)))
       (map :transaction-item/amount)
       (reduce +)))

(defn credit-debit-balanced?
  "Returns a boolean value indicating whether or not the credit and debit totals are equal in the specified items"
  [items]
  (let [credit-total (item-total :transaction-item.action/credit items)
        debit-total (item-total :transaction-item.action/debit items)]
    (= credit-total debit-total)))

(defn valid-transaction-data?
  "Returns a boolean value indicating whether or not the transaction data is valid"
  [{transaction-date :transaction/date description :transaction/description items :transaction/items}]
  (cond
    (nil? transaction-date) false
    (nil? description) false
    (not (credit-debit-balanced? items)) false
    :else true))
