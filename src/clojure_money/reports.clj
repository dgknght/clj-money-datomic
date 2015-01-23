(ns clojure-money.reports
  (:use clojure-money.accounts
        clojure.set)
  (:gen-class))

;; This class shouldn't reference any database functions directly
;; Rather, it should compose HOF from accounts and transactions
;; to create the reports

(declare interleave-summaries)
(defn balance-sheet-report
  "Returns a balance sheet report"
  [db as-of-date]
  (->> (all-accounts db)
       (sort-by :account/type)
       (partition-by :account/type)
       (interleave-summaries)))

(defn entity-map->hash-map
  [entity]
  (apply hash-map (-> entity
                      seq
                      flatten)))

(defn format-account
  "Takes an entity map of an account and formats it for a report"
  [account]
  (-> account
      entity-map->hash-map
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
(defn interleave-summaries
  "Takes a list of accounts grouped by account type interleaves account type summaries"
  [grouped-accounts]
  (reduce (fn [result group]
            (let [total (reduce (fn [total account] (+ total (:account/balance account))) 0 group)
                  header {:caption ((:account/type (first group)) account-type-caption-map)
                          :value total
                          :style :header
                          :depth 0}
                  formatted-group (format-accounts group)]
              (apply vector (concat (conj result header) formatted-group))))
          []
          grouped-accounts))
