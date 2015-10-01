(ns clj-money.test-common
  (:require [datomic.api :as d :refer [delete-database create-database transact]]
            [clj-money.admin :as admin])
  (:use clj-money.common))

(def test-uri "datomic:mem://money")

(defn create-empty-db
  []
  (admin/create-empty-db test-uri))
