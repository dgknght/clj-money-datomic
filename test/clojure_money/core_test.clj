(ns clojure-money.core-test
  (:require [expectations :refer :all]
            [clojure-money.core :refer :all]
            [datomic.api :as d :refer [delete-database create-database transact]]))

;TODO Need to figoure out how to handle different URIs for different environments
(def uri "datomic:mem://money")

(defn create-empty-db []
  (d/delete-database uri)
  (d/create-database uri)
  (let [conn (d/connect uri)]
    (d/transact conn schema)
    conn))
