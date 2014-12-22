(ns clojure-money.transactions-test
  (:require [expectations :refer :all]
            [clojure-money.core :refer [conn]]
            [clojure-money.core-test :refer [create-empty-db]]
            [clojure-money.accounts :refer :all]
            [clojure-money.transactions :refer :all]))

;; When I add a transaction, it appears in the list of transactions
(expect (more-> #datetime "2014-12-15" :transaction/date
                "Paycheck" :transaction/description
                (bigdec 1000) (-> :transaction/items first :transaction-item/amount)
                :transaction-item.action/debit (-> :transaction/items first :transaction-item/action)
                :transaction-item.action/credit (-> :transaction/items second :transaction-item/action))
        (with-redefs [conn (create-empty-db)]
          (do
            (add-account "Checking" :account.type/asset)
            (add-account "Salary" :account.type/income)
            (add-transaction #datetime "2014-12-15"
                             "Paycheck"
                             [[:transaction-item.action/debit "Checking" (bigdec 1000)]
                              [:transaction-item.action/credit "Salary" (bigdec 1000)]])
            (first (get-transactions #datetime "2014-12-01" #datetime "2014-12-31")))))

;; A transaction must be in balance in order to be saved
(expect IllegalArgumentException
        (with-redefs [conn (create-empty-db)]
          (do
            (add-account "Checking" :account.type/asset)
            (add-account "Salary" :account.type/income)
            (add-transaction #datetime "2014-12-15"
                             "Paycheck"
                             [[:transaction-item.action/debit "Checking" (bigdec 1000)]
                              [:transaction-item.action/credit "Salary" (bigdec 500)]]))))
;; When I add a transaction, it should affect the balance of the referenced accounts
