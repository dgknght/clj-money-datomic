(ns clj-money.transactions-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
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
    (add-account conn account-def))
  conn)

(defn new-test-db
  "Creates and populates a test db"
  []
  (-> (create-empty-db)
      add-test-accounts))

(deftest add-a-transaction
  (testing "When I add a transaction, it appears in the list of transactions"
    (let [conn (new-test-db)
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
    (let [conn (new-test-db)
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
    (let [conn (new-test-db)]
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
    (let [conn (new-test-db)
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
    (let [conn (new-test-db)
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

(defn create-update-test-transaction
  "Creates a transaction used in the update tests"
  [conn]
  (add-account conn "Checking")
  (add-account conn {:account/name "Salary" :account/type :account.type/income})
  (add-transaction conn {:transaction/date #inst "2015-01-01"
                         :transaction/description "Not the right description"
                         :transaction/items [{:transaction-item/account "Checking"
                                              :transaction-item/amount (bigdec 1000)
                                              :transaction-item/action :transaction-item.action/debit}
                                             {:transaction-item/account "Salary"
                                              :transaction-item/amount (bigdec 1000)
                                              :transaction-item/action :transaction-item.action/credit}]}))

(deftest update-a-transaction
  (testing "After I update a transaction, I can read the new values back from storage"
    (let [conn (create-empty-db)
          _ (create-update-test-transaction conn)
          transaction (-> conn d/db get-transactions first (into {}))
          updated-trans (assoc transaction :transaction/description "Paycheck")
          _ (update-transaction conn updated-trans)
          all-transactions (-> conn d/db get-transactions)]
      (is (= 1 (count all-transactions)))
      (is (= "Paycheck" (-> all-transactions first :transaction/description)))))
  (testing "Account names are resolved to account IDs"
    (let [conn (create-empty-db)
          _ (create-update-test-transaction conn)
          transaction (-> conn d/db get-transactions first (into {}))
          updated-trans (-> transaction
                            (assoc :transaction/description "Paycheck")
                            (update-in [:transaction/items] vec)
                            (assoc-in [:transaction/items 0 :transaction-item/account] "Checking")
                            (assoc-in [:transaction/items 1 :transaction-item/account] "Salary"))
          _ (update-transaction conn updated-trans)
          all-transactions (-> conn d/db get-transactions)]
      (is (= 1 (count all-transactions)))
      (is (= "Paycheck" (-> all-transactions first :transaction/description))))))

(deftest get-transaction-items-for-an-account
  (testing "get-account-transaction-items returns tuples containing the transaction item and the transaction for all transaction items for the account"
    (is false))
  (testing "get-account-transaction-items returns the transaction items in descending order by transaction date"
    (is false)))

(deftest transaction-balance-chain
  (testing "The balance for the first transaction item for an account is the same as its polarized amount"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/date #inst "2015-09-01"
                                          :transaction/description "Paycheck"
                                          :amount (bigdec 1000)
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
          checking (resolve-account (d/db conn) "Checking")
          [transaction-item transaction] (first (get-account-transaction-items (d/db conn) (:db/id checking)))]
      (is (= (bigdec 1000) (:transaction-item/balance transaction-item)))))
  (testing "The transaction item balance is the sum of the polarized amount for the item and the previous transaction item balance"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/date #inst "2015-09-01"
                                          :transaction/description "Paycheck"
                                          :amount (bigdec 1000)
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
          _ (add-simple-transaction conn {:transaction/date #inst "2015-09-02"
                                          :transaction/description "Kroger"
                                          :amount (bigdec 150)
                                          :debit-account "Groceries"
                                          :credit-account "Checking"})
          checking (resolve-account (d/db conn) "Checking")
          [transaction-item transaction] (first (get-account-transaction-items (d/db conn) (:db/id checking)))]
      (is (= (bigdec 150) (:transaction-item/amount transaction-item)))
      (is (= (bigdec 850) (:transaction-item/balance transaction-item))))))
