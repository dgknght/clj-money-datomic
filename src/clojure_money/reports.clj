(ns clojure-money.reports
  (:require [datomic.api :as d :refer [q]])
  (:gen-class))

(defn balance-sheet-report
  "Returns a balance sheet report"
  [db as-of-date]
  (d/q  '[:find ?name
          :where [_ :account/name ?name]]
       db))
