(ns clojure-money.budgets
  (:require [datomic.api :as d :refer [transact q db]]
            [clojure-money.common :as m :refer :all]
            [clojure-money.accounts :refer :all])
  (:gen-class))

(defn all-budgets
  "Returns a list of all budgets in the system"
  [db]
  (->> (d/q
         '[:find ?b
           :where [?b :budget/name]]
         db)
       (map #(m/hydrate-entity db %))))

(defn add-budget
  "Adds a new budget to the system"
  [conn budget-name start-date]
  (let [new-id (d/tempid :db.part/user)
        tx-data {:db/id new-id
                 :budget/name budget-name
                 :budget/start-date start-date}]
    @(d/transact conn [tx-data])))

(defn find-budget-by-name
  "Returns the budget having the specified name, or nil if no such budget was found"
  [db budget-name]
  (->> (d/q
         '[:find ?b
           :in $ ?budget-name
           :where [?b :budget/name ?budget-name]],
         db,
         budget-name)
       first
       (m/hydrate-entity db)))

(defn add-budget-item
  "Adds a line item to a budget"
  [conn budget-or-name account-or-path amounts]
  (let [budget (if (string? budget-or-name)
                 (find-budget-by-name (d/db conn) budget-or-name)
                 budget-or-name)
        account (if (string? account-or-path)
                  (find-account-by-path (d/db conn) account-or-path)
                  account-or-path)
        periods (map-indexed
                  #(hash-map :budget-item-period/index %1 :budget-item-period/amount %2)
                  amounts)
        tx-data [{:db/id (:db/id budget)
                  :budget/items [{:budget-item/account (:db/id account)
                                  :budget-item/periods periods}]}]]

    (if-not budget (throw (IllegalArgumentException. (str "Unable to find a budget named \"" budget-or-name "\""))))
    (if-not account (throw (IllegalArgumentException. (str "Unable to find an account with the path \"" account-or-path "\""))))

    @(d/transact conn tx-data)))
