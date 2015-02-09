(ns clojure-money.test-common
  (:require [expectations :refer :all]
            [datomic.api :as d :refer [delete-database create-database transact]])
  (:use clojure-money.common))

;TODO Need to figoure out how to handle different URIs for different environments
(def test-uri "datomic:mem://money")

(defn create-empty-db []
  (d/delete-database test-uri)
  (d/create-database test-uri)
  (let [conn (d/connect test-uri)]
    (d/transact conn schema)
    conn))
