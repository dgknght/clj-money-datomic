(ns clojure-money.transactions-test
  (:require [expectations :refer :all]
            [datomic.api :as d :refer [db]]
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
          (first (get-transactions (d/db conn) #datetime "2014-12-01" #datetime "2014-12-31"))))

;; When I add a transaction, it returns an ID I can use to retrieve the transaction
(expect (more-> "Paycheck" :transaction/description
                #datetime "2014-12-15" :transaction/date)
        (let [conn (create-empty-db)]
          (add-account conn "Checking" :account.type/asset)
          (add-account conn "Salary" :account.type/income)
          (let [id (add-transaction conn
                                    {:transaction/date #datetime "2014-12-15"
                                     :transaction/description "Paycheck"
                                     :transaction/items [{:transaction-item/action :transaction-item.action/debit
                                                          :transaction-item/account "Checking"
                                                          :transaction-item/amount (bigdec 1000)}
                                                         {:transaction-item/action :transaction-item.action/credit
                                                          :transaction-item/account "Salary"
                                                          :transaction-item/amount (bigdec 1000)}]})]
            (get-transaction (d/db conn) id))))

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
          (let [checking (find-account-id-by-path (d/db conn) "Checking")
                salary (find-account-id-by-path (d/db conn) "Salary")]
            (add-simple-transaction conn {:transaction/date #datetime "2014-12-15"
                                          :transaction/description "Paycheck"
                                          :amount (bigdec 1000)
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
            [(get-balance (d/db conn) checking) (get-balance (d/db conn) salary)])))

;; When I credit an asset account, the balance should decrease
;; When I debit an expense account, the balance should increase
(expect [(bigdec 1500) (bigdec 500)]
        (let [conn (create-empty-db)]
          (add-account conn "Checking" :account.type/asset)
          (add-account conn "Salary" :account.type/income)
          (add-account conn "Rent" :account.type/expense)
          (let [checking (find-account-id-by-path (d/db conn) "Checking")
                rent (find-account-id-by-path (d/db conn) "Rent")]
            (add-simple-transaction conn {:transaction/date #datetime "2014-12-15"
                                          :transaction/description "Paycheck"
                                          :amount (bigdec 2000)
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
            (add-simple-transaction conn {:transaction/date #datetime "2014-12-15"
                                          :transaction/description "Rent"
                                          :amount (bigdec 500)
                                          :debit-account "Rent"
                                          :credit-account "Checking"})
            [(get-balance (d/db conn) checking) (get-balance (d/db conn) rent)])))

;; When I debit a liability account, the balance should decrease

;; When I credit a liability account, the balance should increase
(expect (bigdec 500)
        (let [conn (create-empty-db)]
          (add-account conn "Credit card" :account.type/liability)
          (add-account conn "Rent" :account.type/expense)
          (let [credit-card (find-account-id-by-path (d/db conn) "Credit card")
                rent (find-account-id-by-path (d/db conn) "Rent")]
            (add-simple-transaction conn {:transaction/date #datetime "2014-12-15"
                                          :transaction/description "Rent"
                                          :amount (bigdec 500)
                                          :debit-account "Rent"
                                          :credit-account "Credit card"})
            (get-balance (d/db conn) credit-card))))

;; When I debit an equity account, the balance should decrease

;; When I credit an equity account, the balance should increase

;; When I debit an income account, the balance should decrease



;; When I credit an expense account, the balance should descease

;; Adding a simple transaction should affect the account balances properly
(expect [(bigdec 1000) (bigdec 1000)]
        (let [conn (create-empty-db)]
          (add-account conn "Checking" :account.type/asset)
          (add-account conn "Salary" :account.type/income)
          (let [db (d/db conn)
                checking (find-account-id-by-path db "Checking")
                salary (find-account-id-by-path db "Salary")]
            (add-simple-transaction conn {:transaction/date #datetime "2014-02-27"
                                          :transaction/description "Paycheck"
                                          :amount (bigdec 1000)
                                          :credit-account salary
                                          :debit-account checking})
            (let [db (d/db conn)]
              (vector (get-balance db checking) (get-balance db salary))))))

;; Adding a simple transaction should create the correct full transaction
(expect (more-> 2 (-> :transaction/items count))
        (let [conn (create-empty-db)]
          (add-account conn "Checking" :account.type/asset)
          (add-account conn "Salary" :account.type/income)
          (let [db (d/db conn)
                checking (find-account-id-by-path db "Checking")
                salary (find-account-id-by-path db "Salary")]
            (let [id (add-simple-transaction conn {:transaction/date #datetime "2014-02-27"
                                          :transaction/description "Paycheck"
                                          :amount (bigdec 1000)
                                          :credit-account salary
                                          :debit-account checking})
                  db (d/db conn)]
              (get-transaction db id)))))
