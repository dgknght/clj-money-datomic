(ns clojure-money.reports
  (:use clojure-money.common
        clojure-money.accounts
        clojure-money.budgets
        clojure-money.transactions
        clojure-money.util
        clojure.set)
  (:require [clojure.string :as string]
            [clj-time.coerce :as c]
            [clj-time.core :as t])
  (:gen-class))

;; These functions shouldn't reference any database functions directly
;; Rather, it should compose HOF from accounts and transactions
;; to create the reports

(defn strip-unneeded-values
  "Removes values that are unneeded for reporting purposes"
  [report-data]
  (map #(select-keys % [:caption
                        :value
                        :depth
                        :style
                        :budget
                        :difference
                        :percent-difference
                        :actual-per-month]) report-data))

;TODO Remove this?
(defn group-by-type
  "Takes a list of display records and returns a hash with account types as keys,
  removing the account type from each item in the list"
  [display-records]
  (group-by :account/type display-records))

(defn sum-by-type
  "Calculates the sum of root records of the specified type from the specified records"
  [account-type display-records]
  (->> display-records
       (filter #(and (= 0 (:depth %))
                     (= account-type (:account-type %))))
       (reduce #(+ %1 (:value %2)) 0)))

(defn append-retained-earnings
  "Takes a sequence of display records and inserts a 'Retained earnings'
  record of type equity based on the income and expense totals"
  [display-records]
  (let [income (sum-by-type :account.type/income display-records)
        expense (sum-by-type :account.type/expense display-records)
        retained-earnings (- income expense)]
    (conj display-records {:caption "Retained earnings"
                           :path "Retained earnings"
                           :value retained-earnings
                           :style :data
                           :account-type :account.type/equity
                           :depth 0})))

(defn map-keys
  "Takes a map containing datomic keys and returns a map with 
  report-ready keys, omitting unecessary values"
  [account]
  (-> account
      (rename-keys {:account/name :caption :account/balance :value})
      (assoc :style :data)))

(defn sum
  "Returns the sum of the values of the specified records"
  [ks accounts]
  (zipmap ks (map (fn [k] (reduce #(+ %1 (k %2)) 0 accounts)) ks)))

;TODO remove this?
(defn append-totals
  "Takes a hash with account types for keys and a list of display records for values and
  converts the list of display-records into a vector containing the list of accounts and
  the total for the accounts in each group"
  [ks grouped-accounts]
  (reduce (fn [result [k accounts]]
            (let [group-total (->> accounts
                                   (filter #(= 0 (:depth %)))
                                   (sum ks))]
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

(def income-statement-account-types [:account.type/income
                                     :account.type/expense])

(defn interleave-summaries
  "Takes a list of display records and interleaves account type summary records"
  [account-types display-records]
  (let [grouped (group-by :account-type display-records)]
    (reduce (fn [result account-type]
              (let [record-group (account-type grouped)]
                (concat result
                        [{:caption (account-type account-type-caption-map)
                          :depth 0
                          :style :header
                          :value (sum-by-type account-type record-group)}]
                        (sort-by :path record-group))))
            []
            account-types)))

(defn starts-with
  "Compares two strings to see if string-a starts with string-b"
  [string-a string-b]
  (let [len (count string-b)]
  (= (take len string-a)
     (take len string-b))))

(defn child-display-records
  "Extracts the display records for the accounts that are children of the account referenced by the specified display record"
  [{specified-path :path} all-records]
  (filter (fn [{path :path}]
            (and (not= specified-path path)
                 (starts-with path specified-path)))
          all-records))

(declare set-balances)
(defn set-balance
  "Sets the :value for the specified display record for the specified time frame"
  [db from to {account :account :as display-record} all-records]
  (let [calculated (calculate-account-balance db (:db/id account) from to)
        children (set-balances db from to (child-display-records display-record all-records))
        sum-of-children (reduce #(+ %1 (:value %2)) 0 children)
        final-balance (+ calculated sum-of-children)]
    (assoc display-record :value final-balance :children children)))

(defn set-balances
  "Sets the :value values for the specified display records for the specified time frame"
  ([db to display-records] (set-balances db earliest-date to display-records)) ;TODO Shortcut this for balance sheet reports as of the current date
  ([db from to display-records]
   (map #(set-balance db from to % display-records) display-records)))

;TODO remove these?
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

(defn calculate-depth
  "Returns an account's depth based on its path"
  [path]
  (-> path
      (string/split #"/")
      count
      dec))

(defn display-records
  [db]
  (->> (all-accounts db)
       (map #(hash-map :account %
                       :caption (:account/name %)
                       :account-type (:account/type %)
                       :style :data))
       (map (fn [{account :account :as record}]
              (assoc record :path (calculate-path-with-list account all-accounts))))
       (sort-by :path)
       (map (fn [{path :path :as record}]
              (assoc record :depth (calculate-depth path))))))

(defn balance-sheet-report
  "Returns a balance sheet report"
  [db as-of-date]
  (->> (display-records db)
       (set-balances db as-of-date)
       append-retained-earnings
       (interleave-summaries balance-sheet-account-types)))

(defn income-statement-report
  "Returns an income statement report"
  [db from to]
  (->> (display-records db)
       (set-balances db from to)
       (interleave-summaries income-statement-account-types)))

(defn append-budget-amount
  [db budget periods account]
  (let [children (map #(append-budget-amount db budget periods %) (:children account))
        budget-amount (+ (reduce #(+ %1 (:budget %2)) 0 children) (get-budget-amount db budget account periods))]
    (assoc account :budget budget-amount
                   :children children)))

(defn append-analysis
  [{budget-amount :budget actual :value :as row} periods]
  (let [difference (- actual budget-amount)
        percent-difference (if (not= budget-amount 0)
                             (with-precision 3 (/ difference budget-amount)))
        actual-per-month (with-precision 2  (/ actual periods))]
    (assoc row :difference difference
           :percent-difference percent-difference
           :actual-per-month actual-per-month)))

(defn budget-report
  "Returns a budget using the specified budget and including the specified periods"
  [db budget-or-name periods]
  (let [budget (resolve-budget db budget-or-name)]
    (->> (stacked-accounts db)
         (set-balances db (:budget/start-date budget) (budget-end-date budget))
         (map #(append-budget-amount db budget periods %))
         flatten-accounts
         (map map-keys)
         (sort-by :account/type)
         (group-by-type)
         (append-totals [:value :budget])
         (interleave-summaries [:account.type/income :account.type/expense])
         (map #(append-analysis % periods))
         strip-unneeded-values)))

(defn budget-monitor
  "Returns information about the spending level in an account compared to
  the expected spending level based on the number of days elapsed on the month"
  [db account-or-name date]
  (let [account (resolve-account db account-or-name)
        budget (find-budget-containing-date db date)
        period (find-budget-item-period db budget account date)
        budget-amount (:budget-item-period/amount period)
        dt (c/from-date date)
        expected-percent (/ (t/day dt) (t/number-of-days-in-the-month dt))
        actual (calculate-account-balance db (:db/id account) (c/to-date (:start-date period)) date) #_(TODO Standardize the class used to hold date values)]
    {:budget budget-amount
     :expected-percent expected-percent
     :expected (with-precision 3 (* expected-percent budget-amount))
     :actual actual
     :projected (with-precision 3 (/ actual expected-percent))
     :actual-percent (with-precision 2 (/ actual budget-amount))}))
