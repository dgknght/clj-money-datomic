(ns clj-money.test-common
  (:require [datomic.api :as d :refer [delete-database create-database transact]])
  (:use clj-money.common))

(def test-uri "datomic:mem://money")

(defn create-empty-db []
  (d/delete-database test-uri)
  (d/create-database test-uri)
  (let [conn (d/connect test-uri)]
    (d/transact conn schema)
    conn))
