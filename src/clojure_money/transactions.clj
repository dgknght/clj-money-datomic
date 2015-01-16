(ns clojure-money.transactions
  (:require [datomic.api :as d :refer [tempid q db transact pull-many]]
            [clojure-money.core :refer :all]
            [clojure-money.accounts :refer :all])
  (:gen-class))

;; ----- Primary methods -----

(declare resolve-transactions-enums)
(defn get-transactions
  "Returns the transactions in the specified timeframe"
  [db start-date end-date]
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
        (resolve-transactions-enums db)))

(declare resolve-transaction-data)
(declare validate-transaction-data)
(declare create-balance-adjustment-tx-data)
(defn add-transaction
  "Adds a new transaction to the system"
  [conn data]
  (validate-transaction-data data)
  (let [db (d/db conn)
        new-id (d/tempid :db.part/user)
        complete-data (->> data
                           (resolve-transaction-data db)
                           (merge {:db/id new-id}))
        all-tx-data (concat
                      [complete-data]
                      (create-balance-adjustment-tx-data (:transaction/items complete-data)))]
    (let [result @(d/transact conn all-tx-data)
          tempids (:tempids result)]
      (d/resolve-tempid (d/db conn) tempids new-id))))

(defn add-simple-transaction
  "Add a two-item transaction, crediting one account and debiting another"
  [conn {:keys [amount debit-account credit-account] :as data}]
  (let [transaction-tx-data (-> data
                                (dissoc :amount :credit-account :debit-account)
                                (assoc :transaction/items [{:transaction-item/action :transaction-item.action/debit
                                                            :transaction-item/account debit-account
                                                            :transaction-item/amount amount}
                                                           {:transaction-item/action :transaction-item.action/credit
                                                            :transaction-item/account credit-account
                                                            :transaction-item/amount amount}]))]
    (add-transaction conn transaction-tx-data)))

(defn get-transaction
  "Returns a transaction, given a transaction id"
  [db id]
  (d/touch (d/entity db id)))

;; ----- Helper methods -----

(defn create-balance-adjustment-tx-data
  "Adds tx-data for adjusting account balances for a transaction"
  [items]
  (map
    (fn [{account :transaction-item/account action :transaction-item/action amount :transaction-item/amount}]
      [:adjust-balance account amount action])
    items))

(defn resolve-account
  "Resolves the information into an account id. The input may be a path, account entity, or id"
  [db token]
  (cond (string? token) (find-account-by-path db token)
        (integer? token) (d/entity db token)
        :else token))

(defn resolve-transaction-item-data
  "Resolves references inside transaction item data"
  [db data]
  (assoc-in data [:transaction-item/account] (:db/id (resolve-account db (:transaction-item/account data)))))

(defn resolve-transaction-data
  "Resolves references inside transaction data"
  [db data]
  (assoc-in data [:transaction/items] (map #(resolve-transaction-item-data db %) (:transaction/items data))))

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
  [db transactions]
  (map #(resolve-transaction-enums db %) transactions))

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

(defn validate-transaction-data
  "Throws an exception if any of the data is invalid"
  [data]
  (let [errors (reduce (fn [list [test-fn test-msg]]
                          (if (test-fn data) (conj list test-msg)))
                       []
                       [[#(nil? (:transaction/date %)) ":transaction/date must be specified"]
                        [#(nil? (:transaction/description %)) ":transaction/description must be specified"]
                        [#(not (credit-debit-balanced? (:transaction/items %))) "The transaction items must have balanced debit and credit totals"]])]
    (if (seq errors) (throw (IllegalArgumentException. (apply str (concat ["The transaction data is not valid: "] errors)))))))