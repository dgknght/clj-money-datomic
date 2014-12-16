(ns clojure-money.core-test
  (:require [expectations :refer :all]
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

;; After I save an account, it appears in the list of all accounts
(expect #{["Checking" :account.type/asset]}
        (with-redefs [conn (create-empty-db)]
          (do
            (add-account "Checking")
            (all-accounts))))
(expect #{["Checking" :account.type/asset] ["Savings" :account.type/asset]}
        (with-redefs [conn (create-empty-db)]
          (do
            (add-account "Checking")
            (add-account "Savings")
            (all-accounts))))

;; An account can be an asset, liability, equity, income, or expense
(expect #{["Credit card" :account.type/liability]}
        (with-redefs [conn (create-empty-db)]
          (do
            (add-account "Credit card" :account.type/liability)
            (all-accounts))))
(expect Exception
        (with-redefs [conn (create-empty-db)]
          (do
            (add-account "Checking" "notatype")
            (all-accounts))))
