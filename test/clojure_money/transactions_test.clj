(ns clojure-money.transactions-test
  (:require [expectations :refer :all]
            [clojure-money.core-test :refer [create-empty-db]]
            [clojure-money.accounts :refer :all]
            [clojure-money.transactions :refer :all]))

;; When I add a transaction, it appears in the list of transactions
(expect (more-> #datetime "2014-12-15" :transaction/date
                "Paycheck" :transaction/description
                (bigdec 1000) (-> :transaction/items first :transaction-item/amount)
                :transaction-item.action/debit (-> :transaction/items first :transaction-item/action)
                :transaction-item.action/credit (-> :transaction/items second :transaction-item/action))
        (let [conn (create-empty-db)]
          (add-account conn "Checking" :account.type/asset)
          (add-account conn "Salary" :account.type/income)
          (add-transaction conn
                           {:transaction/date #datetime "2014-12-15"
                            :transaction/description "Paycheck"
                            :transaction/items [{:transaction-item/action :transaction-item.action/debit
                                                 :transaction-item/account "Checking"
                                                 :transaction-item/amount (bigdec 1000)}
                                                {:transaction-item/action :transaction-item.action/credit
                                                 :transaction-item/account "Salary"
                                                 :transaction-item/amount (bigdec 1000)}]})
          (first (get-transactions conn #datetime "2014-12-01" #datetime "2014-12-31"))))

;; A transaction must be in balance in order to be saved
(expect IllegalArgumentException
        (let [conn (create-empty-db)]
          (add-account conn "Checking" :account.type/asset)
          (add-account conn "Salary" :account.type/income)
          (add-transaction conn
                           {:transaction/date #datetime "2014-12-15"
                            :transaction/description "Paycheck"
                            :transaction/items [{:transaction-item/action :transaction-item.action/debit
                                                 :transaction-item/account "Checking"
                                                 :transaction-item/amount (bigdec 1000)}
                                                {:transaction-item/action :transaction-item.action/credit
                                                 :transaction-item/account "Salary"
                                                 :transaction-item/amount (bigdec 500)}]})))

;; When I debit an asset account, the balance should increase
;; When I credit an income account, the balance should increase
(expect [(bigdec 1000) (bigdec 1000)]
        (let [conn (create-empty-db)]
          (add-account conn "Checking" :account.type/asset)
          (add-account conn "Salary" :account.type/income)
          (add-transaction conn
                           {:transaction/date #datetime "2014-12-15"
                            :transaction/description "Paycheck"
                            :transaction/items [{:transaction-item/action :transaction-item.action/debit
                                                 :transaction-item/account "Checking"
                                                 :transaction-item/amount (bigdec 1000)}
                                                {:transaction-item/action :transaction-item.action/credit
                                                 :transaction-item/account "Salary"
                                                 :transaction-item/amount (bigdec 1000)}]})
          [(get-balance conn "Checking") (get-balance conn "Salary")]))

;; When I credit an asset account, the balance should decrease

;; When I debit a liability account, the balance should decrease

;; When I credit a liability account, the balance should increase

;; When I debit an equity account, the balance should decrease

;; When I credit an equity account, the balance should increase

;; When I debit an income account, the balance should decrease


;; When I debit an expense account, the balance should increase

;; When I credit an expense account, the balance should descease
