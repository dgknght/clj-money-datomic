(ns clj-money.transactions
  (:require [datomic.api :as d :refer [tempid q db transact pull-many pull]]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clj-time.core :as t]
            [clj-time.coerce :as coerce])
  (:use clj-money.common
        clj-money.accounts
        [clj-money.util :as util])
  (:gen-class))

(def max-date (t/date-time 9999 12 31))
(def min-date (t/date-time 1000  1  5))

(defn resolve-action
  "Looks up a transaction item action from a db/id"
  [db action]
  (if (keyword? action)
    action
    (get-ident db (:db/id action))))

(defn resolve-transaction-item-enums
  "Looks up references in a list of transaction item maps"
  [db item]
  (update item
          :transaction-item/action
         #(resolve-action db %)))

(defn resolve-transaction-items-enums
  "Looks up references in a list of transaction items"
  [db items]
  (mapv #(resolve-transaction-item-enums db %) items))

(defn resolve-transaction-enums
  "Looks up references in transaction map"
  [db transaction]
  (update transaction :transaction/items #(resolve-transaction-items-enums db %)))

(defn resolve-transactions-enums
  "Looks up references in a list of transaction maps"
  [db transactions]
  (map #(resolve-transaction-enums db %) transactions))

(defn get-transaction
  "Returns a transaction, given a transaction id"
  [db id]
  (d/touch (d/entity db id)))

(defn find-transaction-item
  "Given an id, finds the transaction item"
  [db id]
  (->> (d/entity db id)
       d/touch))

(defn lookup-transaction-items
  "Given a transaction, looks up the transaction items"
  [db transaction]
  (update transaction
          :transaction/items
          #(->> % (map :db/id) (pull-many db '[*]))))

(defn get-transactions
  "Returns all transactions"
  ([db]
   (->> (d/q
          '[:find ?t
            :where [?t :transaction/date _]]
          db)
        (map first)
        (pull-many db '[*])
        (map #(lookup-transaction-items db %))
        (resolve-transactions-enums db))))

(defn prepare-transaction-item-query-result
  "Accepts the raw results on a transaction item query, which is a sequence of tuples
  containing the transaction item id and the transaction id, in that order, and
  prepares it for return to the caller"
  [db account-id {sort-order :sort-order :or {sort-order :desc}} raw-result]
  (let [sort-compare (if (= :asc sort-order)
                       compare
                       #(compare %2 %1))]
    (->> raw-result
         (map (fn [tuple]
                (map #(pull db '[*] %) tuple)))
         (sort-by #(-> % first :transaction-item/index) sort-compare))))

(def default-get-account-transaction-item-options {:sort-order :desc
                                                   :inclusive? true})

(defn get-account-transaction-items
  "Returns tramsaction items referencing the specified account.

  The date should be specified as a clj-time (joda) date time. It will be converted to
  a java date for the purpose of the query."
  ([db account-id] (get-account-transaction-items db account-id {}))
  ([db account-id options] (get-account-transaction-items db account-id min-date options))
  ([db account-id start-date options] (get-account-transaction-items db account-id start-date max-date options))
  ([db account-id start-date end-date options]
   (let [options (merge default-get-account-transaction-item-options options)
         query (if (:inclusive? options)
                 '[:find ?ti ?t
                   :in $ ?account-id ?start-date ?end-date
                   :where [?ti :transaction-item/account ?account-id]
                   [?t :transaction/items ?ti]
                   [?t :transaction/date ?transaction-date]
                   [(<= ?start-date ?transaction-date)]
                   [(>= ?end-date ?transaction-date)]]
                 '[:find ?ti ?t
                   :in $ ?account-id ?start-date ?end-date
                   :where [?ti :transaction-item/account ?account-id]
                   [?t :transaction/items ?ti]
                   [?t :transaction/date ?transaction-date]
                   [(< ?start-date ?transaction-date)]
                   [(> ?end-date ?transaction-date)]])]
     (->>  (d/q
             query
             db
             account-id
             (coerce/to-date start-date)
             (coerce/to-date end-date))
          (prepare-transaction-item-query-result db account-id options)))))

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

(defn get-last-transaction-item-before
  [db account-id transaction-date]
  (ffirst (get-account-transaction-items db account-id min-date transaction-date {:sort-order :desc :inclusive? false})))

(defn get-transaction-items-after
  [db account-id transaction-date]
  (mapv first (get-account-transaction-items db account-id transaction-date max-date {:sort-order :asc})))

(defn init-item-processing-context
  "Creates the processing context for a given a transaction item and transaction date.

  The context includes :db :last-balance :last-index :adj-items."

  [db account-id transaction-date]
  (let [{balance :transaction-item/balance
         index   :transaction-item/index} (get-last-transaction-item-before db account-id transaction-date)]
    {:db db
     :last-balance (or balance 0M)
     :last-index (or index -1N)
     :current-items []
     :adj-items []}))

(defn polarized-amount
  "Given a transaction item, returns the amount by which the corresponding
  account balance changes as a result of the transaction item"
  ([db item]
   (polarized-amount db (resolve-account db (:transaction-item/account item) item)))
  ([db account {amount :transaction-item/amount action :transaction-item/action}]
   (* amount (polarizer account action))))

(defn process-item
  [account
   {:keys [last-balance last-index db]
    :as context}
   {amount              :transaction-item/amount
    action              :transaction-item/action
    id                  :db/id
    :as                 item}]
  (let [pol         (polarizer account (resolve-action db action))
        adjustment  (* pol amount)
        new-balance (+ last-balance adjustment)
        new-index   (+ last-index 1)]
    (-> context
        (update :adj-items #(conj % [:db/add id
                                     :transaction-item/balance new-balance]
                                  [:db/add id
                                   :transaction-item/index new-index]))
        (assoc :last-balance new-balance)
        (assoc :last-index new-index))))

(defn process-current-items
  [context account items]
  (reduce (partial process-item account)
          context
          items))

(defn process-after-items
  [{db :db :as context} account-id transaction-date ignore]
  (let [account (find-account db account-id)
        after-items (->> (get-transaction-items-after db account-id transaction-date)
                         (remove #(ignore (:db/id %))))]
    (reduce (partial process-item account)
            context
            after-items)))

(defn dereferenced-account-deltas
  "Given a list of items, returns any account deltas to the given
  list for any items for which the account has changed such that the
  account no longer referended by the transaction item will have
  the correct balance"
  [db items]
  (->> items
       (remove #(map? (:db/id %)))
       (reduce (fn [deltas item]
                 (let [old-item (find-transaction-item db (:db/id item))]
                   (when (not= 1 (->> [old-item item]
                                      (map :transaction-item/account)
                                      (map :db/id)
                                      (into #{})
                                      count))
                     (let [account (:transaction-item/account old-item)
                           delta (- 0 (polarized-amount db account old-item))]
                       (conj deltas [account delta])))))
               [])))

(defn transaction-item-balance-adjustments
  "Given all transaction items for an account withing a transaction,
  account-id, and transaction date, returns a map containing:
  :current-items - the original items with balance and index appended
  :adjusted-items - adjustments to index and balance of all affected existing transaction items
  :account-deltas - the change to be applied to the account and its parents to update the balances"
  [db account-token items transaction-date]
  (let [{account-id :db/id :as account} (resolve-account db account-token)
        unique-item-ids (->> items (map :db/id) (remove nil?) (apply hash-set))
        {:keys [current-items
                last-balance
                adj-items]} (-> (init-item-processing-context db account-id transaction-date)
                                (process-current-items account items)
                                (process-after-items account-id transaction-date unique-item-ids))
        account-adjustment  (- last-balance (:account/balance account))
        adjusted-account    [:db/add account-id :account/balance last-balance]
        account-deltas      (dereferenced-account-deltas db items)]
    {:current-items current-items
     :adjusted-items adj-items
     :adjusted-account adjusted-account
     :account-deltas account-deltas}))

(defn transaction-item-group-adjustments
  [context [account-id items]]
  (let [{:keys [current-items
                adjusted-items
                adjusted-account
                account-deltas]} (transaction-item-balance-adjustments (:db context)
                                                                       account-id
                                                                       items
                                                                       (:transaction-date context))]
    (-> context
        (update :current-items concat current-items)
        (update :adjusted-items concat adjusted-items)
        (update :adjusted-accounts conj adjusted-account)
        (update :account-deltas concat account-deltas))))

(defn finalize-account-adjustments
  [account-deltas]
  (->> account-deltas
       (reduce (fn [acc [account delta]]
                 (if (contains? acc account)
                   (update acc account + delta)
                   (assoc acc account delta)))
               {})
       (map (fn [[account adjustment]]
              [:db/add (:db/id account)
               :account/balance (+ (:account/balance account) adjustment)]))))

(defn append-balance-adjustment-tx-data
  "Appends the datomic transaction commands necessary to adjust balances 
  for the transaction"
  [db {items :transaction/items transaction-date :transaction/date :as transaction}]
  (let [context (reduce transaction-item-group-adjustments
                        {:db db
                         :current-items []
                         :adjusted-items []
                         :adjusted-accounts []
                         :account-deltas []
                         :transaction-date transaction-date}
                        (group-by :transaction-item/account items))
        account-adjustments (-> context :account-deltas finalize-account-adjustments)]
    (cons transaction
          (concat (:adjusted-items context)
                  (:adjusted-accounts context)
                  account-adjustments))))

(defn resolve-transaction-item-data
  "Resolves references inside transaction item data"
  [db data]
  (assoc-in data [:transaction-item/account] (:db/id (resolve-account db (:transaction-item/account data)))))

(defn resolve-transaction-data
  "Resolves references inside transaction data"
  [data db]
  (assoc-in data [:transaction/items] (map #(resolve-transaction-item-data db %) (:transaction/items data))))

(defn prepare-transaction-data
  "Takes the raw transaction data and makes it ready use with d/transact"
  [db data]
  (-> data
      (resolve-transaction-data db)
      (update :transaction/date #(coerce/to-date %))
      (update :transaction/items (fn [items]
                                   (map #(assoc % :db/id (d/tempid :db.part/user)) items)))))

(defn add-transaction
  "Adds a new transaction to the system"
  [conn {items :transaction/items :as data}]
  (validate-transaction-data data)
  (let [db (d/db conn)
        new-id (d/tempid :db.part/user)
        tx-data (->> data
                     (prepare-transaction-data db)
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

(defn remove-balance-and-index
  "Removes the existing balance and index attributes from line
  items so that they can be recalculated as part of an update"
  [transaction]
  (update-in transaction
             [:transaction/items]
             (fn [items]
               (map #(dissoc % :transaction-item/balance :transaction-item/index) items))))

(defn update-transaction
  "Updates an existing transaction in the system"
  [conn data]
  (let [db (d/db conn)
        tx-data (->> data
                     (resolve-references db)
                     (remove-balance-and-index)
                     (append-balance-adjustment-tx-data db))]
    @(d/transact conn tx-data)))

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
