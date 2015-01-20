(ns clojure-money.reports
  (:require [datomic.api :as d :refer :all])
  (:gen-class))

(defn balance-sheet-report
  "Returns a balance sheet report"
  [db as-of-date]
  nil)
