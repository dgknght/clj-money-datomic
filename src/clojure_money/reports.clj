(ns clojure-money.reports
  (:use clojure-money.accounts)
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

(defn interleave-summaries
  "Takes a list of accounts grouped by account type interleaves account tyep summaries"
  [grouped-accounts]
  (reduce (fn [result group]
            (let [total (reduce (fn [total account] (+ total (:account/balance account))) 0 group)]
              (concat (conj result {:caption (:account/type (first group))
                                  :value total
                                  :style :header
                                  :depth 0})
                    group)))
          []
          grouped-accounts))
