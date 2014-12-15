(ns clojure-money.core-test
  (:require [clojure.test :refer :all]
            [clojure-money.core :refer :all]
            [datomic.api :as d :refer :all]))

;TODO Need to figoure out how to handle different URIs for different environments
(def uri "datomic:mem://money")

(defn create-empty-db []
  (d/delete-database uri)
  (d/create-database uri)
  (let [conn (d/connect uri)]
    (d/transact conn schema)
    conn))

(deftest a-saved-account-can-be-retrieved
  (with-redefs [conn (create-empty-db)]
    (testing "After I save an account, I can retrieve it"
      (add-account "Checking")
      (printf "all-accounts returns %s\n" (all-accounts))
      (is (= [["Checking"]] (all-accounts))))))
