(ns clojure-money.budgets
  (:require [datomic.api :as d :refer [transact q db]]
            [clojure-money.common :as m :refer :all])
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
