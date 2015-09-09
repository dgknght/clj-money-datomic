(ns clojure-money.reports
  (:use clojure-money.common
        clojure-money.accounts
        clojure-money.budgets
        clojure-money.transactions
        clojure-money.util
        clojure.set)
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clj-time.coerce :as c]
            [clj-time.core :as t])
  (:gen-class))

;; These functions shouldn't reference any database functions directly
;; Rather, it should compose HOF from accounts and transactions
;; to create the reports

(def root-accounts-only
  (filter #(= 0 (:depth %))))

(defn accounts-of-type
  "Returns a transducer that filters the records by account type"
  [account-type]
  (filter #(= account-type (:account-type %))))

(defn extract-value
  "Maps the display-records to a single extracted value"
  [k]
  (map #(k %)))

(defn sum-root-accounts-by-type
  "Returns a transducer for summing display record values with roll up by account type"
  [account-type field-key]
  (comp (accounts-of-type account-type)
        root-accounts-only
        (extract-value field-key)))

(defn sum-root-accounts
  "Returns a transducer for summing display record values with roll up"
  [field-key]
  (comp root-accounts-only
        (extract-value field-key)))

(defn sum-accounts
  "Returns a transducer for summing display record values"
  [field-key]
  (extract-value field-key))

(defn append-retained-earnings
  "Takes a sequence of display records and inserts a 'Retained earnings'
  record of type equity based on the income and expense totals"
  [display-records]
  (let [income (transduce (sum-root-accounts-by-type :account.type/income :value) + display-records)
        expense (transduce (sum-root-accounts-by-type :account.type/expense :value) + display-records)
        retained-earnings (- income expense)]
    (conj display-records {:caption "Retained earnings"
                           :path "Retained earnings"
                           :value retained-earnings
                           :style :data
                           :account-type :account.type/equity
                           :depth 0})))

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
(defn process-record-summary
  [{:keys [keys-to-sum
           totals-are-rolled-up
           sort-key
           summary-prep-fn]
    :or {keys-to-sum [:value]
         totals-are-rolled-up false
         sort-key :path
         summary-prep-fn identity}}
   [account-type raw-record-group]]
  (let [record-group (sort-by sort-key raw-record-group)
        x-fn (if totals-are-rolled-up
               sum-root-accounts
               sum-accounts)
        summary-base {:caption (account-type account-type-caption-map)
                      :path (account-type account-type-caption-map) ;TODO Can we eliminate this redundancy?
                      :depth 0
                      :style :header}
        totals (->> keys-to-sum
                    (mapcat #(vector [% (transduce (x-fn %) + record-group)]))
                    (into {}))
        summary (-> summary-base
                     (merge totals)
                     summary-prep-fn)]
    (cons summary record-group)))

(defn interleave-summaries
  "Takes a list of display records and interleaves account type summary records"
  [{:keys [account-types] :as options}
   display-records]
  (let [grouped (group-by :account-type display-records)]
    (->> account-types
         (map #(vector % (% grouped)))
         (mapcat #(process-record-summary options %)))))

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

(declare set-balances-with-rollup)
(defn set-balance-with-rollup
  "Sets the :value for the specified display record for the specified time frame"
  [db from to {account :account :as display-record} all-records]
  (let [calculated (calculate-account-balance db (:db/id account) from to)
        children (set-balances-with-rollup db from to (child-display-records display-record all-records))
        sum-of-children (reduce #(+ %1 (:value %2)) 0 children)
        final-balance (+ calculated sum-of-children)]
    (assoc display-record :value final-balance)))

(defn set-balances-with-rollup
  "Sets the :value values for the specified display records for the specified time frame"
  ([db to display-records] (set-balances-with-rollup db earliest-date to display-records)) ;TODO Shortcut this for balance sheet reports as of the current date
  ([db from to display-records]
   (map #(set-balance-with-rollup db from to % display-records) display-records)))

(defn set-balance
  "Sets the balance of the display record without rolling values up to parent rows"
  [db from to {{account-id :db/id} :account :as display-record}]
  (assoc display-record
         :value
         (calculate-account-balance db account-id from to)))

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
       (set-balances-with-rollup db as-of-date)
       append-retained-earnings
       (interleave-summaries {:account-types balance-sheet-account-types
                              :totals-are-rolled-up true})))

(defn income-statement-report
  "Returns an income statement report"
  [db from to]
  (->> (display-records db)
       (set-balances-with-rollup db from to)
       (interleave-summaries {:account-types income-statement-account-types
                              :totals-are-rolled-up true})))

(defn append-budget-amount
  [db budget periods {account :account :as display-record}]
  (assoc display-record :budget (get-budget-amount db budget account periods)))

(defn append-analysis
  [{:keys [budget value] :as display-record} periods]
  (let [difference (- value budget)
        percent-difference (if-not (= budget (bigdec 0))
                             (with-precision 3 (/ difference budget)))
        actual-per-month (with-precision 2  (/ value periods))]
    (assoc display-record :difference difference
           :percent-difference percent-difference
           :actual-per-month actual-per-month)))

(defn budget-report
  "Returns a budget using the specified budget and including the specified periods"
  [db budget-or-name periods]
  (let [{budget-start :budget/start-date :as budget} (resolve-budget db budget-or-name)
        budget-end (budget-end-date budget)]
    (->> (display-records db)
         (filter #(#{:account.type/income :account.type/expense} (:account-type %)))
         (map #(set-balance db budget-start budget-end  %))
         (map #(append-budget-amount db budget periods %))
         (remove (fn [{:keys [value budget]}] (= (bigdec 0) value budget)))
         (map #(append-analysis % periods))
         (interleave-summaries {:account-types [:account.type/income :account.type/expense]
                                :keys-to-sum [:value :budget]
                                :sort-key :difference
                                :summary-prep-fn #(append-analysis % periods)}))))

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
