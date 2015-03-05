(ns clojure-money.budgets-test
  (:require [expectations :refer :all]
            [datomic.api :as d :refer [db]]
            [clojure-money.budgets :refer :all])
  (:use clojure-money.test-common))

;; When I add a budget, it should appear in the list of budgets
(expect [{:budget/name "2015" :budget/start-date #inst "2015-01-01"}]
        (let [conn (create-empty-db)]
          (add-budget conn "2015" #inst "2015-01-01")
          (map #(into {} %) (all-budgets (d/db conn)))))
