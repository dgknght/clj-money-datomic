(ns clojure-money.budgets-test
  (:require [expectations :refer :all]
            [datomic.api :as d :refer [db]]
            [clojure-money.budgets :refer :all])
  (:use clojure-money.test-common))

;; When I add a budget, it should appear in the list of budgets
(expect ["2015"]
        (let [conn (create-empty-db)]
          (add-budget conn "2015")
          (map :budget/name (all-budgets (d/db conn)))))
