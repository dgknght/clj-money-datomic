(ns clj-money.transactions
  (:require [datomic.api :as d :refer [tempid q db transact pull-many]]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clj-time.core :as t]
            [clj-time.coerce :as coerce])
  (:use clj-money.common
        clj-money.accounts
        [clj-money.util :as util])
  (:gen-class))

;; ----- Primary methods -----

(def max-date (t/date-time 9999 12 31))
(def min-date (t/date-time 1000  1  5))

(declare resolve-transactions-enums)
(defn get-transactions
  "Returns all transactions"
  ([db]
   (->> (d/q
          '[:find ?t
            :where [?t :transaction/date _]]
          db)
        (map first)
        (pull-many db '[*])
        (resolve-transactions-enums db))))

(defn prepare-transaction-item-query-result
  "Accepts the raw results on a transaction item query and prepares it for return to the caller"
  [db account-id {sort-order :sort-order :or {sort-order :desc}} raw-result]
  (let [sort-compare (if (= :asc sort-order)
                       compare
                       #(compare %2 %1))]
    (->> raw-result
         (map first)
         (pull-many db '[*])
         (reduce (fn [result {items :transaction/items :as transaction}]
                   (concat result
                           (->> items
                                (filter #(= account-id (-> % :transaction-item/account :db/id)))
                                (map #(vector % transaction)))))
                 [])
         (sort-by #(-> % second :transaction/date) sort-compare))))

(defn get-account-transaction-items
  "Returns a sequence of tuples containing the transaction item and the transaction for
  all transaction items referencing the specified account.

  The date should be specified as a clj-time (joda) date time. It will be converted to
  a java date for the purpose of the query."
  ([db account-id] (get-account-transaction-items db account-id {}))
  ([db account-id options] (get-account-transaction-items db account-id min-date options))
  ([db account-id start-date options] (get-account-transaction-items db account-id start-date max-date options))
  ([db account-id start-date end-date options]
   (->>  (d/q
           '[:find ?t
             :in $ ?account-id ?start-date ?end-date
             :where [?ti :transaction-item/account ?account-id]
             [?t :transaction/items ?ti]
             [?t :transaction/date ?transaction-date]
             [(<= ?start-date ?transaction-date)]
             [(>= ?end-date ?transaction-date)]]
           db
           account-id
           (coerce/to-date start-date)
           (coerce/to-date end-date))
        (prepare-transaction-item-query-result db account-id options))))

(defn get-account-transaction-items-for-transaction-adjustment
  "Returns all transaction items for an account in ascending chronological order starting with the
  first one that is on or before the specified date"
  [db account-id transaction-date]
  (->>  (d/q
          '[:find ?t
            :in $ ?account-id ?new-transaction-date
            :where [?ti :transaction-item/account ?account-id]
            [?t :transaction/items ?ti]
            [?t :transaction/date ?transaction-date]
            [(>= ?transaction-date ?new-transaction-date)]]
          db
          account-id
          (coerce/to-date transaction-date))
       (prepare-transaction-item-query-result db account-id {:sort-order :asc})))

(declare resolve-transaction-data)
(declare validate-transaction-data)

; TODO Remove this function
(defn adjust-account-balances
  "Adds tx-data for adjusting account balances for a transaction"
  [conn items]
  (doseq [{account :transaction-item/account action :transaction-item/action amount :transaction-item/amount} items]
         (adjust-balance conn account amount action)))

(defn transaction-item-balance-adjustments
  "Creates tx data necessary to adjust transaction item and account balances as the 
  result of the specified transaction item data. The return value is a tuple containing
  the original item data (with the balance attribute added) in the 1st position and a sequence
  of tx data to update the corresponding account balance (and any other affected transaction items)
  in the 2nd position."
  [db {amount :transaction-item/amount
       action :transaction-item/action
       account-id :transaction-item/account
       :as item} transaction-date]

  ; TODO Refactor out the redundancies between this let and the nested let
  (let [account (find-account db account-id)
        pol (polarizer account action)
        adjustment (* pol amount)
        related-items (map first (get-account-transaction-items-for-transaction-adjustment db account-id transaction-date))
        before-item (first related-items)
        after-items (rest related-items)
        before-balance (if before-item
                         (:transaction-item/balance before-item)
                         (bigdec 0)) 
        balance (+ before-balance adjustment)]
    [(assoc item :transaction-item/balance balance)
     (:result (reduce (fn [x {amount :transaction-item/amount account :transaction-item/account action :transaction-item/action}]
                        (let [account (find-account db (:db/id account))
                              pol (polarizer account action)
                              adjustment (* pol amount)
                              new-balance (+ (:last-balance x) adjustment)]
                          (-> x
                              (update :result #(conj % [:db/add (:db/id item)
                                                        :transaction-item/balance new-balance]))
                              (assoc :last-balance new-balance))))
                      {:last-balance balance :result []}
                      after-items))]))

(defn append-balance-adjustment-tx-data
  "Appends the datomic transaction commands necessary to adjust balances 
  for the transaction"
  [db {items :transaction/items transaction-date :transaction/date :as transaction}]
  (let [final-result (reduce (fn [result item]
                               (let [[adj-item adjustments] (transaction-item-balance-adjustments db item transaction-date)]
                                 (-> result
                                     (update :items conj adj-item)
                                     (update :adjustments concat adjustments))))
                             {:items [] :adjustments []}
                             items)]
    (cons (assoc transaction :transaction/items (:items final-result))
          (:adjustments final-result))))

(defn add-transaction
  "Adds a new transaction to the system"
  [conn {items :transaction/items :as data}]
  (validate-transaction-data data)
  (let [db (d/db conn)
        new-id (d/tempid :db.part/user)
        tx-data (->> data
                     (resolve-transaction-data db)
                     (merge {:db/id new-id})
                     (append-balance-adjustment-tx-data db))
        result @(d/transact conn tx-data)
        tempids (:tempids result)]
    (d/resolve-tempid (d/db conn) tempids new-id)))

(defn resolve-references
  "Looks up account references in the transaction data and replaced when with entity ID values"
  [db transaction-data]
  (update transaction-data
          :transaction/items
          (fn [items]
            (map (fn [item] (if (string? (:transaction-item/account item))
                              (update item :transaction-item/account (partial resolve-account-id db))
                              item))
                 items))))

(defn update-transaction
  "Updates an existing transaction in the system"
  [conn data]
  @(d/transact conn [(resolve-references (d/db conn) data)]))

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

(defn calculate-account-balance
  "Given an account ID, totals the transaction item values for the specified account through the specified date"
  ([db account as-of-date] (calculate-account-balance db account earliest-date as-of-date))
  ([db account from to]
  (let [account (d/touch (d/entity db account))
        amounts (d/q '[:find ?amount ?action-name ?i ; The ?i value isn't used, but without it, duplicate rows were not returned
                       :in $ ?from ?to ?account-id
                       :where [?t :transaction/date ?transaction-date]
                       [?t :transaction/items ?i]
                       [?i :transaction-item/account ?account-id]
                       [?i :transaction-item/amount ?amount]
                       [?i :transaction-item/action ?action]
                       [?action :db/ident ?action-name]
                       [(<= ?transaction-date ?to)]
                       [(>= ?transaction-date ?from)]]
                     db
                     from
                     to
                     (:db/id account))]
    (reduce (fn [result [amount action]]
              (+ result (* amount (polarizer account action))))
            (bigdec 0)
            amounts))))

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
