(ns clj-money.transactions-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [datomic.api :as d :refer [db]]
            [clj-time.core :refer [now]])
  (:use clj-money.test-common
        clj-money.accounts
        clj-money.transactions
        clj-money.util))

(def account-defs [{:account/name "Checking"
                    :account/type :account.type/asset}
                   {:account/name "Salary"
                    :account/type :account.type/income}
                   {:account/name "Groceries"
                    :account/type :account.type/expense}
                   {:account/name "Taxes"
                    :account/type :account.type/expense}
                   {:account/name "Federal"
                    :account/type :account.type/expense
                    :account/parent "Taxes"}
                   {:account/name "Social security"
                    :account/type :account.type/expense
                    :account/parent "Taxes"}
                   {:account/name "Medicare"
                    :account/type :account.type/expense
                    :account/parent "Taxes"}
                   {:account/name "Opening balances"
                    :account/type :account.type/equity}
                   {:account/name "Credit card"
                    :account/type :account.type/liability}])

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
  (let [conn (create-empty-db)
        _ (add-account conn "Checking")
        _ (add-account conn {:account/name "Salary" :account/type :account.type/income})
        _ (add-account conn {:account/name "Rent" :account/type :account.type/expense})
        _ (add-simple-transaction conn {:transaction/date #inst "2015-09-02"
                                        :transaction/description "Rent"
                                        :amount (bigdec 450)
                                        :debit-account "Rent"
                                        :credit-account "Checking"})
        _ (add-simple-transaction conn {:transaction/date #inst "2015-09-01"
                                        :transaction/description "Paycheck"
                                        :amount (bigdec 1000)
                                        :debit-account "Checking"
                                        :credit-account "Salary"})
        _ (add-simple-transaction conn {:transaction/date #inst "2015-09-15"
                                        :transaction/description "Paycheck"
                                        :amount (bigdec 1000)
                                        :debit-account "Checking"
                                        :credit-account "Salary"})
        _ (add-simple-transaction conn {:transaction/date #inst "2015-08-15"
                                        :transaction/description "Paycheck"
                                        :amount (bigdec 1000)
                                        :debit-account "Checking"
                                        :credit-account "Salary"})
        checking-id (resolve-account-id (d/db conn) "Checking")
        result (get-account-transaction-items (d/db conn) checking-id)]
    (testing "get-account-transaction-items returns tuples containing the transaction item and the transaction for all transaction items for the account"
      (is (= 4 (-> result count)))
      (is (= 2 (-> result first count)))
      (is (contains? (-> result ffirst) :transaction-item/account))
      (is (contains? (-> result first second) :transaction/date)))
    (testing "get-account-transaction-items returns the transaction items in descending order by transaction date"
      (is (= [#inst "2015-09-15" #inst "2015-09-02" #inst "2015-09-01" #inst "2015-08-15"]
             (map #(-> % second :transaction/date) result))))
    (testing "get-account-transaction-items returns the transaction items in ascending order by transaction date if that option is specified"
      (let [asc-result (get-account-transaction-items (d/db conn) checking-id {:sort-order :asc})]
        (is (= [#inst "2015-08-15" #inst "2015-09-01" #inst "2015-09-02" #inst "2015-09-15"]
               (map #(-> % second :transaction/date) asc-result)))))
    (testing "When a single date is specified, it returns all transactions since (and including) that date"
      (let [since-9-1 (get-account-transaction-items (d/db conn) checking-id #inst "2015-09-01" {})]
        (is (= [#inst "2015-09-15" #inst "2015-09-02" #inst "2015-09-01"]
               (map #(-> % second :transaction/date) since-9-1)))))
    (testing "When two dates are specified, it returns all transactions between the two dates, inclusively"
      (let [between-dates (get-account-transaction-items (d/db conn) checking-id #inst "2015-08-01" #inst "2015-09-01" {})]
        (is (= [#inst "2015-09-01" #inst "2015-08-15"]
               (map #(-> % second :transaction/date) between-dates)))))))

;;                  Debit     Credit
;; Asset            Increase  Decrease
;; Liability        Decrease  Increase
;; Income/Revenue   Decrease  Increase
;; Expense          Increase  Decrease
;; Equity/Capital   Decrease  Increase

(deftest debit-calculations
  (testing "Debiting an asset account increases the balance"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/description "Paycheck"
                                          :transaction/date (now)
                                          :amount (bigdec 100)
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
          balance (get-balance (d/db conn) "Checking")]
      (is (= (bigdec 100) balance))))
  (testing "Debiting an expense account increases the balance"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/description "Kroger"
                                          :transaction/date (now)
                                          :amount (bigdec 101)
                                          :debit-account "Groceries"
                                          :credit-account "Checking"})
          balance (get-balance (d/db conn) "Groceries")]
      (is (= (bigdec 101) balance))))
  (testing "Debiting a liability account decreases the balance"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/description "Pay credit card bill"
                                          :transaction/date (now)
                                          :amount (bigdec 102)
                                          :debit-account "Credit card"
                                          :credit-account "Checking"})
          balance (get-balance (d/db conn) "Credit card")]
      (is (= (bigdec -102) balance))))
  (testing "Debiting an equity account decreases the balance"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/description "Credit card opening balances"
                                          :transaction/date (now)
                                          :amount (bigdec 103)
                                          :debit-account "Opening balances"
                                          :credit-account "Credit card"})
          balance (get-balance (d/db conn) "Opening balances")]
      (is (= (bigdec -103) balance))))
  (testing "Debiting an income account decreases the balance"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/description "Had to give back some salary for bad behavior"
                                          :transaction/date (now)
                                          :amount (bigdec 104)
                                          :debit-account "Salary"
                                          :credit-account "Checking"})
          balance (get-balance (d/db conn) "Salary")]
      (is (= (bigdec -104) balance)))))

(deftest credit-calculations 
  (testing "Crediting an asset account decreases the balance"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/description "Kroger"
                                          :transaction/date (now)
                                          :amount (bigdec 105)
                                          :debit-account "Groceries"
                                          :credit-account "Checking"})
          balance (get-balance (d/db conn) "Checking")]
      (is (= (bigdec -105) balance))))
  (testing "Crediting an expense account decreases the balance"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/description "Kroger refund for bad milk"
                                          :transaction/date (now)
                                          :amount (bigdec 106)
                                          :debit-account "Checking"
                                          :credit-account "Groceries"})
          balance (get-balance (d/db conn) "Groceries")]
      (is (= (bigdec -106) balance))))
  (testing "Crediting a liability account increases the balance"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/description "Kroger"
                                          :transaction/date (now)
                                          :amount (bigdec 107)
                                          :debit-account "Groceries"
                                          :credit-account "Credit card"})
          balance (get-balance (d/db conn) "Credit card")]
        (is (= (bigdec 107) balance))))
    (testing "Crediting an equity account increases the balance"
      (let [conn (new-test-db)
            _ (add-simple-transaction conn {:transaction/description "Checking opening balance"
                                            :transaction/date (now)
                                            :amount (bigdec 108)
                                            :debit-account "Checking"
                                            :credit-account "Opening balances"})
            balance (get-balance (d/db conn) "Opening balances")]
        (is (= (bigdec 108) balance))))
    (testing "Crediting an income account increases the balance"
      (let [conn (new-test-db)
            _ (add-simple-transaction conn {:transaction/description "Paycheck"
                                            :transaction/date (now)
                                            :amount (bigdec 109)
                                            :debit-account "Checking"
                                            :credit-account "Salary"})
            balance (get-balance (d/db conn) "Salary")]
        (is (= (bigdec 109) balance)))))

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
      (is (= (bigdec 850) (:transaction-item/balance transaction-item)))))
  (testing "Adding a transaction before other transactions causes the following transactions' balances to be updated"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/date #inst "2015-09-15"
                                          :transaction/description "Paycheck"
                                          :amount (bigdec 1000)
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
          _ (add-simple-transaction conn {:transaction/date #inst "2015-09-16"
                                          :transaction/description "Kroger"
                                          :amount (bigdec 100)
                                          :debit-account "Groceries"
                                          :credit-account "Checking"})
          checking-id (resolve-account-id (d/db conn) "Checking")
          before (get-account-transaction-items (d/db conn) checking-id)
          _ (add-simple-transaction conn {:transaction/date #inst "2015-09-01"
                                          :transaction/description "Paycheck"
                                          :amount (bigdec 1000)
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
          after (get-account-transaction-items (d/db conn) checking-id)]
      (is (= [[#inst "2015-09-16" (bigdec 900)]
              [#inst "2015-09-15" (bigdec 1000)]]
             (map #(vector (-> % second :transaction/date)
                           (-> % first :transaction-item/balance)) before)))
      (is (= [[#inst "2015-09-16" (bigdec 1900)]
              [#inst "2015-09-15" (bigdec 2000)]
              [#inst "2015-09-01" (bigdec 1000)]]
             (map #(vector (-> % second :transaction/date)
                           (-> % first :transaction-item/balance)) after))))))

(deftest transaction-item-indexing
  (testing "adding a transaction with a date after the last item puts the item at the end (front) of the account items with an index one greater than the pervious last"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/date #inst "2015-09-01"
                                          :transaction/description "Paycheck"
                                          :amount (bigdec 1000)
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
          _ (add-simple-transaction conn {:transaction/date #inst "2015-09-02"
                                          :transaction/description "Kroger"
                                          :amount (bigdec 100)
                                          :debit-account "Groceries"
                                          :credit-account "Checking"})
          account (resolve-account (d/db conn) "Checking")
          items (get-account-transaction-items (d/db conn) (:db/id account))]
      (is (= [1 0] (->> items
                        (map first)
                        (map #(:transaction-item/index %)))))
      (is (= [#inst "2015-09-02" #inst "2015-09-01"] (->> items
                                                       (map second)
                                                       (map #(:transaction/date %)))))))
  (testing "adding a transaction item before the last item for an account updates the index of the last item of the account"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/date #inst "2015-09-02"
                                          :transaction/description "Kroger"
                                          :amount (bigdec 100)
                                          :debit-account "Groceries"
                                          :credit-account "Checking"})
          _ (add-simple-transaction conn {:transaction/date #inst "2015-09-01"
                                          :transaction/description "Paycheck"
                                          :amount (bigdec 1000)
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
          account (resolve-account (d/db conn) "Checking")
          items (get-account-transaction-items (d/db conn) (:db/id account))
          expected [{:transaction-item/index 1
                     :transaction/date #inst "2015-09-02"
                     :transaction-item/balance (bigdec 900)}
                    {:transaction-item/index 0
                     :transaction/date #inst "2015-09-01"
                     :transaction-item/balance (bigdec 1000)}]
          actual (map (fn [[i t]]
                        (merge (select-keys i [:transaction-item/index :transaction-item/balance])
                               (select-keys t [:transaction/date])))
                      items)]
      (is (= expected
             actual))))
  (testing "A new item is inserted before other items with the same date"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/date #inst "2015-09-01"
                                          :transaction/description "Kroger"
                                          :amount (bigdec 100)
                                          :debit-account "Groceries"
                                          :credit-account "Checking"})
          _ (add-simple-transaction conn {:transaction/date #inst "2015-09-01"
                                          :transaction/description "Paycheck"
                                          :amount (bigdec 1000)
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
          account (resolve-account (d/db conn) "Checking")
          items (get-account-transaction-items (d/db conn) (:db/id account))
          expected [{:transaction-item/index 1
                     :transaction/date #inst "2015-09-01"
                     :transaction-item/balance (bigdec 900)}
                    {:transaction-item/index 0
                     :transaction/date #inst "2015-09-01"
                     :transaction-item/balance (bigdec 1000)}]
          actual (map (fn [[i t]]
                        (merge (select-keys i [:transaction-item/index :transaction-item/balance])
                               (select-keys t [:transaction/date])))
                      items)]
      (is (= expected
             actual))))
  (testing "When a transaction with multiple items referencing the same account is saved, the transaction items are handled correctly."
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/date #inst "2015-01-01"
                                          :transaction/description "Paycheck"
                                          :amount (bigdec 1000)
                                          :credit-account "Salary"
                                          :debit-account "Checking"})
          _ (add-simple-transaction conn {:transaction/date #inst "2015-02-01"
                                          :transaction/description "Paycheck"
                                          :amount (bigdec 1000)
                                          :credit-account "Salary"
                                          :debit-account "Checking"})
          _ (add-transaction conn {:transaction/date #inst "2015-01-15"
                                   :transaction/description "Paycheck with bonus"
                                   :transaction/items [{:transaction-item/action :transaction-item.action/credit
                                                        :transaction-item/account "Salary"
                                                        :transaction-item/amount (bigdec 1000)}
                                                       {:transaction-item/action :transaction-item.action/credit
                                                        :transaction-item/account "Salary"
                                                        :transaction-item/amount (bigdec 500)}
                                                       {:transaction-item/action :transaction-item.action/debit
                                                        :transaction-item/account "Checking"
                                                        :transaction-item/amount (bigdec 1500)}]})
          salary-id (resolve-account-id (d/db conn) "Salary")
          actual (->> (get-account-transaction-items (d/db conn) salary-id)
                      (map #(merge (first %) (second %)))
                      (map #(select-keys % [:transaction/date :transaction-item/amount :transaction-item/balance])))
          expected [{:transaction/date #inst "2015-02-01"
                     :transaction-item/amount (bigdec 1000)
                     :transaction-item/balance (bigdec 3500)}
                    {:transaction/date #inst "2015-01-15"
                     :transaction-item/amount (bigdec 500)
                     :transaction-item/balance (bigdec 2500)}
                    {:transaction/date #inst "2015-01-15"
                     :transaction-item/amount (bigdec 1000)
                     :transaction-item/balance (bigdec 2000)}
                    {:transaction/date #inst "2015-01-01"
                     :transaction-item/amount (bigdec 1000)
                     :transaction-item/balance (bigdec 1000)}]]
      (is (= expected actual)))
    (testing "A transaction with multiple items referencing children of the same account is handled correctly"
      (let [conn (new-test-db)
            _ (add-transaction conn {:transaction/date #inst "2015-01-01"
                                     :transaction/description "Paycheck"
                                     :transaction/items [{:transaction-item/action :transaction-item.action/credit
                                                          :transaction-item/account "Salary"
                                                          :transaction-item/amount (bigdec 1000)}
                                                         {:transaction-item/action :transaction-item.action/debit
                                                          :transaction-item/account "Taxes/Federal"
                                                          :transaction-item/amount (bigdec 150)}
                                                         {:transaction-item/action :transaction-item.action/debit
                                                          :transaction-item/account "Taxes/Social security"
                                                          :transaction-item/amount (bigdec 62)}
                                                         {:transaction-item/action :transaction-item.action/debit
                                                          :transaction-item/account "Taxes/Medicare"
                                                          :transaction-item/amount (bigdec 14.50)}
                                                         {:transaction-item/action :transaction-item.action/debit
                                                          :transaction-item/account "Checking"
                                                          :transaction-item/amount (bigdec 773.50)}]})
            children-balance (:account/children-balance (find-account (d/db conn) "Taxes"))]
        (is (= 226.50 children-balance))))))
