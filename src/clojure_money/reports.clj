(ns clojure-money.reports
  (:use clojure-money.common
        clojure-money.accounts
        clojure-money.transactions
        clojure.set)
  (:gen-class))

;; These functions shouldn't reference any database functions directly
;; Rather, it should compose HOF from accounts and transactions
;; to create the reports

(defn strip-unneeded-values
  "Removes values that are unneeded for reporting purposes"
  [report-data]
  (map #(select-keys % [:caption :value :depth :style]) report-data))

(defn group-by-type
  "Takes a list of accounts and returns a hash with account types as keys,
  removing the account type from each item in the list"
  [accounts]
  (group-by :account/type accounts))

(defn calculate-retained-earnings
  "Takes a map of accounts grouped by type and inserts a 'Retained earnings'
  entry into the equity accounts based on the income and expense values"
  [{[_ income] :account.type/income
    [_ expense] :account.type/expense
    :as grouped-accounts}]
  (let [retained-earnings (- income expense)]
    (-> grouped-accounts
        (update-in [:account.type/equity 0] #(conj % {:caption "Retained earnings"
                                                      :value retained-earnings
                                                      :style :data
                                                      :depth 0}))
        (update-in [:account.type/equity 1] #(+ % retained-earnings)))))

(defn map-keys
  "Takes a map containing datomic keys and returns a map with 
  report-ready keys, omitting unecessary values"
  [account]
  (-> account
      (rename-keys {:account/name :caption :account/balance :value})
      (select-keys [:caption :value :account/type :depth])
      (assoc :style :data)))

(defn sum
  "Returns the sum of the values of the specified records"
  [accounts]
  (reduce #(+ %1 (:value %2)) 0 accounts))

;; Input looks like
;; {:account.type/asset [{:caption "Checking" :value 100 :depth 0 :style :data}
;;                       {:caption "Savings"  :value 150 :depth 0 :style :data}]}
;; Output looks like
;; {:account.type/asset [[{:caption "Checking" :value 100 :depth 0 :style :data}
;;                        {:caption "Savings"  :value 150 :depth 0 :style :data}] 250]}
(defn append-totals
  "Takes a hash with account types for keys and a list of accounts for values and
  converts the list of accounts into a vector containing the list of accounts and
  the total for the accounts in each group"
  [grouped-accounts]
  (reduce (fn [result [k accounts]]
            (let [group-total (->> accounts
                                   (filter #(= 0 (:depth %)))
                                   sum)]
              (assoc result k [accounts group-total])))
          {}
          grouped-accounts))

(defn format-account
  "Takes an entity map of an account and formats it for a report"
  [account]
  (-> account
      (rename-keys {:account/name :caption :account/balance :value})
      (select-keys [:caption :value])
      (assoc :depth 0 :style :data)))

(defn format-accounts
  "Takes a list of accounts and formats them for a report"
  [accounts]
  (map format-account accounts))

(def account-type-caption-map {:account.type/asset "Assets"
                               :account.type/liability "Liabilities"
                               :account.type/equity "Equity"
                               :account.type/income "Income"
                               :account.type/expense "Expense"})

(def balance-sheet-account-types [:account.type/asset
                                  :account.type/liability
                                  :account.type/equity])

;; Input data is a hash map where each key is an account type and each value
;; is a vector containing the list of accounts and the sum of the account values
;; {account-type [vector-of-accounts sum-of-account-values]}
;; Output data is a list of hash maps that look like this
;; {:caption "Assets" :value 2000 :depth 0 :style :header}
(defn interleave-summaries
  "Takes a list of accounts grouped by account type interleaves account type summaries"
  [account-types grouped-accounts]
  (reduce (fn [result t]
            (apply vector (concat (conj result {:caption (t account-type-caption-map)
                                                :value (last (t grouped-accounts))
                                                :style :header
                                                :depth 0})
                                  (first (t grouped-accounts)))))
          []
          account-types))

(declare set-balances)
(defn set-balance
  "Gets the balance for the specified account over the specified time"
  [db from to account]
  (let [calculated (calculate-account-balance db (:db/id account) from to)
        children (set-balances db from to (:children account))
        sum-of-children (reduce #(+ %1 (:account/balance %2)) 0 children)
        final-balance (+ calculated sum-of-children)]
    (assoc account :account/balance final-balance :children children)))

(defn set-balances
  "Sets the :account/balance value for each account based on the specified date"
  ([db to accounts] (set-balances db earliest-date to accounts))
  ([db from to accounts]
   (map #(set-balance db from to %) accounts)))

(declare flatten-accounts)
(defn flatten-account
  "Accepts an account with a :children attribute and returns a list containing 
  the specified account, followed by the children"
  [account depth]
  (let [children (flatten-accounts (:children account) (inc depth))]
    (cons (assoc (dissoc account :children) :depth depth)
          children)))

(defn flatten-accounts
  "Accepts stacked accounts and returns a flat list with a new depth attribute"
  ([accounts] (flatten-accounts accounts 0))
  ([accounts depth]
   (reduce #(concat %1 (flatten-account %2 depth)) [] accounts)))

(defn balance-sheet-report
  "Returns a balance sheet report"
  [db as-of-date]
  (->> (stacked-accounts db)
       (set-balances db as-of-date)
       flatten-accounts
       (map map-keys)
       group-by-type
       append-totals
       calculate-retained-earnings
       (interleave-summaries balance-sheet-account-types)
       strip-unneeded-values))

(defn income-statement-report
  "Returns an income statement report"
  [db from to]
  (->> (stacked-accounts db)
       (set-balances db from to)
       flatten-accounts
       (map map-keys)
       (sort-by :account/type)
       (group-by-type)
       append-totals
       (interleave-summaries [:account.type/income :account.type/expense])
       strip-unneeded-values))
