(ns clj-money.transactions-test
  (:require [clojure.test :refer :all]
            [datomic.api :as d :refer [db]])
  (:use clj-money.test-common
        clj-money.accounts
        clj-money.transactions))

(def account-defs [{:account/name "Checking"
                    :account/type :account.type/asset}
                   {:account/name "Salary"
                    :account/type :account.type/income}
                   {:account/name "Groceries"
                    :account/type :account.type/expense}])

(defn add-test-accounts
  "Creates accounts needed for the tests"
  [conn]
  (doseq [account-def account-defs]
    (add-account conn account-def)))

(deftest add-a-transaction
  (testing "When I add a transaction, it appears in the list of transactions"
    (let [conn (create-empty-db)
          _ (add-test-accounts conn)
          _ (add-transaction conn
                             {:transaction/date #inst "2014-12-15"
                              :transaction/description "Paycheck"
                              :transaction/items [{:transaction-item/action :transaction-item.action/debit
                                                   :transaction-item/account "Checking"
                                                   :transaction-item/amount (bigdec 1000)}
                                                  {:transaction-item/action :transaction-item.action/credit
                                                   :transaction-item/account "Salary"
                                                   :transaction-item/amount (bigdec 1000)}]})
          transactions (get-transactions (d/db conn))
          transaction (first transactions)]
      (is (= 1 (count transactions)))
      (is (= #inst "2014-12-15" (:transaction/date transaction)))
      (is (= "Paycheck" (:transaction/description transaction)))
      (is (= (bigdec 1000) (-> transaction :transaction/items first :transaction-item/amount)))
      (is (= :transaction-item.action/debit (-> transaction :transaction/items first :transaction-item/action)))
      (is (= :transaction-item.action/credit (-> transaction :transaction/items second :transaction-item/action)))))
  (testing "An ID is returned that can be used to retrieve the transaction"
    (let [conn (create-empty-db)
          _ (add-test-accounts conn)
          id (add-transaction conn
                              {:transaction/date #inst "2014-12-15"
                               :transaction/description "Paycheck"
                               :transaction/items [{:transaction-item/action :transaction-item.action/debit
                                                    :transaction-item/account "Checking"
                                                    :transaction-item/amount (bigdec 1000)}
                                                   {:transaction-item/action :transaction-item.action/credit
                                                    :transaction-item/account "Salary"
                                                    :transaction-item/amount (bigdec 1000)}]})
          transaction (get-transaction (d/db conn) id)]
      (is (= "Paycheck" (:transaction/description transaction))))))

(deftest a-transaction-must-be-in-balance
  (testing "An imbalanced transaction cannot be saved"
    (let [conn (create-empty-db)
          _ (add-test-accounts conn)]
      (is (thrown-with-msg? IllegalArgumentException #"The transaction items must have balanced debit and credit totals"
                            (add-transaction conn
                                             {:transaction/date #inst "2014-12-15"
                                              :transaction/description "Paycheck"
                                              :transaction/items [{:transaction-item/action :transaction-item.action/debit
                                                                   :transaction-item/account "Checking"
                                                                   :transaction-item/amount (bigdec 1000)}
                                                                  {:transaction-item/action :transaction-item.action/credit
                                                                   :transaction-item/account "Salary"
                                                                   :transaction-item/amount (bigdec 500)}]}))))))

(deftest add-a-simple-transaction
  (testing "Adding a simple transaction creates a full transaction"
    (let [conn (create-empty-db)
          _ (add-test-accounts conn)
          db (d/db conn)
          checking (find-account-id-by-path db "Checking")
          salary (find-account-id-by-path db "Salary")
          id (add-simple-transaction conn {:transaction/date #inst "2014-02-27"
                                           :transaction/description "Paycheck"
                                           :amount (bigdec 1000)
                                           :credit-account salary
                                           :debit-account checking})
          db (d/db conn)
          transaction (get-transaction db id)]
      (is (= 2 (-> transaction :transaction/items count)))))
  (testing "Adding a simple transaction should affect the account balances properly"
    (let [conn (create-empty-db)
          _ (add-test-accounts conn)
          db (d/db conn)
          checking (find-account-id-by-path db "Checking")
          salary (find-account-id-by-path db "Salary")
          _ (add-simple-transaction conn {:transaction/date #inst "2014-02-27"
                                          :transaction/description "Paycheck"
                                          :amount (bigdec 1000)
                                          :credit-account salary
                                          :debit-account checking})
          db (d/db conn)
          balances (map #(get-balance db %) [checking salary])]
      (is (= [(bigdec 1000) (bigdec 1000)] balances)))))

(defn calculate-account-balance-setup
  "Add transaction for calculate-account-balance tests"
  [conn]
  (add-test-accounts conn)
  (let [db (d/db conn)
        salary (find-account-id-by-path db "Salary")
        checking (find-account-id-by-path db "Checking")
        groceries (find-account-id-by-path db "Groceries")]
    (add-simple-transaction conn {:transaction/date #inst "2015-01-01"
                                  :transaction/description "Paycheck"
                                  :amount (bigdec 1000)
                                  :debit-account checking
                                  :credit-account salary})
    (add-simple-transaction conn {:transaction/date #inst "2015-01-04"
                                  :transaction/description "Kroger"
                                  :amount (bigdec 100)
                                  :debit-account groceries
                                  :credit-account checking})
    (add-simple-transaction conn {:transaction/date #inst "2015-01-11"
                                  :transaction/description "Kroger"
                                  :amount (bigdec 100)
                                  :debit-account groceries
                                  :credit-account checking})
    (add-simple-transaction conn {:transaction/date #inst "2015-01-12"
                                  :transaction/description "Kroger"
                                  :amount (bigdec 10)
                                  :debit-account checking
                                  :credit-account groceries})
    {:checking checking :salary salary :groceries groceries}))

(deftest calculate-the-balance-of-an-account
  (testing "The balance is zero before any transactions take place"
    (let [conn (create-empty-db)
          accounts (calculate-account-balance-setup conn)
          balance (calculate-account-balance (d/db conn) (:checking accounts) #inst "2014-12-31")]
      (is (= (bigdec 0) balance))))
  (testing "The balance includes transactions that happen before the specified date"
    (let [conn (create-empty-db)
          accounts (calculate-account-balance-setup conn)
          balance-01-01 (calculate-account-balance (d/db conn) (:checking accounts) #inst "2015-01-01")
          balance-01-04 (calculate-account-balance (d/db conn) (:checking accounts) #inst "2015-01-04")
          balance-01-11 (calculate-account-balance (d/db conn) (:checking accounts) #inst "2015-01-11")
          balance-12-31 (calculate-account-balance (d/db conn) (:checking accounts) #inst "2015-12-31")]
      (is (= (bigdec 1000) balance-01-01))
      (is (= (bigdec 900) balance-01-04))
      (is (= (bigdec 800) balance-01-11))
      (is (= (bigdec 810) balance-12-31)))))
