(ns clojure-money.accounts-test
  (:require [expectations :refer :all]
            [clojure-money.core :refer [conn]]
            [clojure-money.accounts :refer :all]
            [clojure-money.core-test :refer [create-empty-db]]))

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
