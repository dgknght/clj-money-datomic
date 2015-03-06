(ns clojure-money.budgets-test
  (:require [expectations :refer :all]
            [datomic.api :as d :refer [db touch]]
            [clojure-money.budgets :refer :all]
            [clojure-money.accounts :refer :all]
            [clojure-money.common :refer :all])
  (:use clojure-money.test-common))

(defn prepare-db
  "Prepares the database for budget tests"
  []
  (let [conn (create-empty-db)]
    (add-account conn "Salary" :account.type/income)
    (add-account conn "Groceries" :account.type/expense)
    (add-account conn "Checking" :account.type/asset)
    conn))

;; When I add a budget, it should appear in the list of budgets
(expect [{:budget/name "2015" :budget/start-date #inst "2015-01-01"}]
        (let [conn (create-empty-db)]
          (add-budget conn "2015" #inst "2015-01-01")
          (map #(into {} %) (all-budgets (d/db conn)))))

;; When I add a budget, I should be able to find it by name
(expect {:budget/name "2016" :budget/start-date #inst "2016-01-01"}
        (let [conn (create-empty-db)]
          (add-budget conn "2016" #inst "2016-01-01")
          (into {} (find-budget-by-name (d/db conn) "2016"))))

;; When I add an item to a budget, it should appear in the budget items attribute
(expect [300M 300M 300M 300M 300M 300M 300M 300M 300M 300M 300M 300M]
        (let [conn (prepare-db)]
          (add-budget conn "2015" #inst "2015-01-01")
          (add-budget-item conn "2015" "Groceries" (repeat 12 (bigdec 300)))
          (let [first-item (-> conn
                               d/db
                               (find-budget-by-name "2015")
                               :budget/items
                               first)]
            (->> first-item
                 :budget-item/periods
                 (sort-by :budget-item-period/index)
                 (map :budget-item-period/amount)))))
; I couldn't figure out how to test both of these in one expectation
(expect "Groceries"
        (let [conn (prepare-db)]
          (add-budget conn "2015" #inst "2015-01-01")
          (add-budget-item conn "2015" "Groceries" (repeat 12 (bigdec 300)))
          (let [first-item (-> conn
                               d/db
                               (find-budget-by-name "2015")
                               :budget/items
                               first)]
            (->> first-item
                 :budget-item/account
                 d/touch
                 :account/name))))

;; If I add a budget item for an account that already has a budget item,
;; a new budget item is not created
(expect 1
        (let [conn (prepare-db)]
          (add-budget conn "2015" #inst "2015-01-01")
          (add-budget-item conn "2015" "Groceries" (repeat 12 (bigdec 100)))
          (add-budget-item conn "2015" "Groceries" (repeat 12 (bigdec 300)))
          (-> conn
              d/db
              (find-budget-by-name "2015")
              :budget/items
              count)))

;; If I add a budget item for an account that already has a budget item,
;; the specified values overwrite the existing values
(expect-focused [300M 300M 300M 300M 300M 300M 300M 300M 300M 300M 300M 300M]
        (let [conn (prepare-db)]
          (add-budget conn "2015" #inst "2015-01-01")
          (add-budget-item conn "2015" "Groceries" (repeat 12 (bigdec 100)))
          (add-budget-item conn "2015" "Groceries" (repeat 12 (bigdec 300)))
          (let [first-item (-> conn
                               d/db
                               (find-budget-by-name "2015")
                               :budget/items
                               first)]
            (->> first-item
                 :budget-item/periods
                 (sort-by :budget-item-period/index)
                 (map :budget-item-period/amount)))))
