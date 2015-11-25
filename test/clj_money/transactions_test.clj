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
                   {:account/name "Gasoline"
                    :account/type :account.type/expense}
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
                                                   :transaction-item/amount 1000M}
                                                  {:transaction-item/action :transaction-item.action/credit
                                                   :transaction-item/account "Salary"
                                                   :transaction-item/amount 1000M}]})
          transactions (get-transactions (d/db conn))
          transaction (first transactions)]
      (is (= 1 (count transactions)))
      (is (= #inst "2014-12-15" (:transaction/date transaction)))
      (is (= "Paycheck" (:transaction/description transaction)))
      (is (= 1000M (-> transaction :transaction/items first :transaction-item/amount)))
      (is (= :transaction-item.action/debit (-> transaction :transaction/items first :transaction-item/action)))
      (is (= :transaction-item.action/credit (-> transaction :transaction/items second :transaction-item/action)))))
  (testing "An ID is returned that can be used to retrieve the transaction"
    (let [conn (new-test-db)
          id (add-transaction conn
                              {:transaction/date #inst "2014-12-15"
                               :transaction/description "Paycheck"
                               :transaction/items [{:transaction-item/action :transaction-item.action/debit
                                                    :transaction-item/account "Checking"
                                                    :transaction-item/amount 1000M}
                                                   {:transaction-item/action :transaction-item.action/credit
                                                    :transaction-item/account "Salary"
                                                    :transaction-item/amount 1000M}]})
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
                                                                   :transaction-item/amount 1000M}
                                                                  {:transaction-item/action :transaction-item.action/credit
                                                                   :transaction-item/account "Salary"
                                                                   :transaction-item/amount 500M}]}))))))

(deftest add-a-simple-transaction
  (testing "Adding a simple transaction creates a full transaction"
    (let [conn (new-test-db)
          db (d/db conn)
          checking (find-account-id-by-path db "Checking")
          salary (find-account-id-by-path db "Salary")
          id (add-simple-transaction conn {:transaction/date #inst "2014-02-27"
                                           :transaction/description "Paycheck"
                                           :amount 1000M
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
                                          :amount 1000M
                                          :credit-account salary
                                          :debit-account checking})
          db (d/db conn)
          balances (map #(get-balance db %) [checking salary])]
      (is (= [1000M 1000M] balances)))))

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
                                  :amount 1000M
                                  :debit-account checking
                                  :credit-account salary})
    (add-simple-transaction conn {:transaction/date #inst "2015-01-04"
                                  :transaction/description "Kroger"
                                  :amount 100M
                                  :debit-account groceries
                                  :credit-account checking})
    (add-simple-transaction conn {:transaction/date #inst "2015-01-11"
                                  :transaction/description "Kroger"
                                  :amount 100M
                                  :debit-account groceries
                                  :credit-account checking})
    (add-simple-transaction conn {:transaction/date #inst "2015-01-12"
                                  :transaction/description "Kroger"
                                  :amount 10M
                                  :debit-account checking
                                  :credit-account groceries})
    {:checking checking :salary salary :groceries groceries}))

(deftest calculate-the-balance-of-an-account
  (testing "The balance is zero before any transactions take place"
    (let [conn (create-empty-db)
          accounts (calculate-account-balance-setup conn)
          balance (calculate-account-balance (d/db conn) (:checking accounts) #inst "2014-12-31")]
      (is (= 0M balance))))
  (testing "The balance includes transactions that happen before the specified date"
    (let [conn (create-empty-db)
          accounts (calculate-account-balance-setup conn)
          balance-01-01 (calculate-account-balance (d/db conn) (:checking accounts) #inst "2015-01-01")
          balance-01-04 (calculate-account-balance (d/db conn) (:checking accounts) #inst "2015-01-04")
          balance-01-11 (calculate-account-balance (d/db conn) (:checking accounts) #inst "2015-01-11")
          balance-12-31 (calculate-account-balance (d/db conn) (:checking accounts) #inst "2015-12-31")]
      (is (= 1000M balance-01-01))
      (is (= 900M balance-01-04))
      (is (= 800M balance-01-11))
      (is (= 810M balance-12-31)))))

(defn create-update-test-transaction
  "Creates a transaction used in the update tests"
  [conn]
  (add-account conn "Checking")
  (add-account conn {:account/name "Salary" :account/type :account.type/income})
  (add-transaction conn {:transaction/date #inst "2015-01-01"
                         :transaction/description "Not the right description"
                         :transaction/items [{:transaction-item/account "Checking"
                                              :transaction-item/amount 1000M
                                              :transaction-item/action :transaction-item.action/debit}
                                             {:transaction-item/account "Salary"
                                              :transaction-item/amount 1000M
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
  (testing "When a transaction item amount is updated, following transaction item balances are updated"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/date #inst "2015-01-01"
                                          :transaction/description "Paycheck"
                                          :amount 1000M
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
          _ (add-simple-transaction conn {:transaction/date #inst "2015-01-15"
                                          :transaction/description "Paycheck"
                                          :amount 1000M
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
          checking-before (resolve-account (d/db conn) "Checking")
          tx (->> (get-transactions (d/db conn))
                  (filter #(= #inst "2015-01-01" (:transaction/date %)))
                  first)
          updated-tx (-> tx
                         (assoc-in [:transaction/items 0 :transaction-item/amount] 1100M)
                         (assoc-in [:transaction/items 1 :transaction-item/amount] 1100M))
          _ (update-transaction conn updated-tx)
          fetched-tx (->> (get-transactions (d/db conn))
                          (filter #(= #inst "2015-01-15" (:transaction/date %)))
                          first)]
      (is (= 2100M (-> fetched-tx :transaction/items first :transaction-item/balance)))
      (is (= 2100M (-> fetched-tx :transaction/items second :transaction-item/balance)))))
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
      (is (= "Paycheck" (-> all-transactions first :transaction/description)))))
  (testing "When a transaction item amount is updated, the corresponding account balance is updated"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/date #inst "2015-01-01"
                                          :transaction/description "Paycheck"
                                          :amount 1000M
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
          checking-before (resolve-account (d/db conn) "Checking")
          tx (first (get-transactions (d/db conn)))
          updated-tx (-> tx
                         (assoc-in [:transaction/items 0 :transaction-item/amount] 1100M)
                         (assoc-in [:transaction/items 1 :transaction-item/amount] 1100M))
          _ (update-transaction conn updated-tx)
          checking (resolve-account (d/db conn) "Checking")
          salary (resolve-account (d/db conn) "Salary")]
      (is (= 1100M (:account/balance checking)))
      (is (= 1100M (:account/balance salary)))))
  (testing "When a transaction date is updated, all transaction item indexes are updated"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/date #inst "2015-01-04"
                                          :transaction/description "Kroger"
                                          :amount 100M
                                          :debit-account "Groceries"
                                          :credit-account "Checking"})
          _ (add-simple-transaction conn {:transaction/date #inst "2015-01-11"
                                          :transaction/description "Kroger"
                                          :amount 100M
                                          :debit-account "Groceries"
                                          :credit-account "Checking"})
          _ (add-simple-transaction conn {:transaction/date #inst "2015-01-15"
                                          :transaction/description "Paycheck"
                                          :amount 1000M
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
          tx (->> (get-transactions (d/db conn))
                  (filter #(= #inst "2015-01-15" (:transaction/date %)))
                  first)
          updated-tx (assoc tx :transaction/date #inst "2015-01-01")
          _ (update-transaction conn updated-tx)
          checking (resolve-account (d/db conn) "Checking")
          checking-items (->> (get-account-transaction-items (d/db conn) (:db/id checking))
                              (map #(vector (-> % second :transaction/date)
                                            (-> % first :transaction-item/index)
                                            (-> % first :transaction-item/balance))))]
      (is (= [[#inst "2015-01-11" 2 800M]
              [#inst "2015-01-04" 1 900M]
              [#inst "2015-01-01" 0 1000M]]
             checking-items)))))

(deftest update-a-transaction-item-account
  (testing "When a transaction item account is updated, balances are adjusted for the old account and the new account"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/date #inst "2015-01-01"
                                          :transaction/description "Paycheck"
                                          :amount 1000M
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
          _ (add-simple-transaction conn {:transaction/date #inst "2015-01-04"
                                          :transaction/description "Kroger"
                                          :amount 100M
                                          :debit-account "Groceries"
                                          :credit-account "Checking"})
          tx (->> (get-transactions (d/db conn))
                  (filter #(= #inst "2015-01-04" (:transaction/date %)))
                  first)
          gasoline (resolve-account (d/db conn) "Gasoline")
          updated-tx (assoc-in tx [:transaction/items 0 :transaction-item/account] (:db/id gasoline))
          _ (update-transaction conn updated-tx)
          gasoline-after (resolve-account (d/db conn) "Gasoline")
          groceries-after (resolve-account (d/db conn) "Groceries")]
      (is (= 0M (:account/balance groceries-after)))
      (is (= 100M (:account/balance gasoline-after))))))

(deftest get-transaction-items-for-an-account
  (let [conn (create-empty-db)
        _ (add-account conn "Checking")
        _ (add-account conn {:account/name "Salary" :account/type :account.type/income})
        _ (add-account conn {:account/name "Rent" :account/type :account.type/expense})
        _ (add-simple-transaction conn {:transaction/date #inst "2015-09-02"
                                        :transaction/description "Rent"
                                        :amount 450M
                                        :debit-account "Rent"
                                        :credit-account "Checking"})
        _ (add-simple-transaction conn {:transaction/date #inst "2015-09-01"
                                        :transaction/description "Paycheck"
                                        :amount 1000M
                                        :debit-account "Checking"
                                        :credit-account "Salary"})
        _ (add-simple-transaction conn {:transaction/date #inst "2015-09-15"
                                        :transaction/description "Paycheck"
                                        :amount 1000M
                                        :debit-account "Checking"
                                        :credit-account "Salary"})
        _ (add-simple-transaction conn {:transaction/date #inst "2015-08-15"
                                        :transaction/description "Paycheck"
                                        :amount 1000M
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
                                          :amount 100M
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
          balance (get-balance (d/db conn) "Checking")]
      (is (= 100M balance))))
  (testing "Debiting an expense account increases the balance"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/description "Kroger"
                                          :transaction/date (now)
                                          :amount 101M
                                          :debit-account "Groceries"
                                          :credit-account "Checking"})
          balance (get-balance (d/db conn) "Groceries")]
      (is (= 101M balance))))
  (testing "Debiting a liability account decreases the balance"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/description "Pay credit card bill"
                                          :transaction/date (now)
                                          :amount 102M
                                          :debit-account "Credit card"
                                          :credit-account "Checking"})
          balance (get-balance (d/db conn) "Credit card")]
      (is (= -102M balance))))
  (testing "Debiting an equity account decreases the balance"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/description "Credit card opening balances"
                                          :transaction/date (now)
                                          :amount 103M
                                          :debit-account "Opening balances"
                                          :credit-account "Credit card"})
          balance (get-balance (d/db conn) "Opening balances")]
      (is (= -103M balance))))
  (testing "Debiting an income account decreases the balance"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/description "Had to give back some salary for bad behavior"
                                          :transaction/date (now)
                                          :amount 104M
                                          :debit-account "Salary"
                                          :credit-account "Checking"})
          balance (get-balance (d/db conn) "Salary")]
      (is (= -104M balance)))))

(deftest credit-calculations 
  (testing "Crediting an asset account decreases the balance"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/description "Kroger"
                                          :transaction/date (now)
                                          :amount 105M
                                          :debit-account "Groceries"
                                          :credit-account "Checking"})
          balance (get-balance (d/db conn) "Checking")]
      (is (= -105M balance))))
  (testing "Crediting an expense account decreases the balance"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/description "Kroger refund for bad milk"
                                          :transaction/date (now)
                                          :amount 106M
                                          :debit-account "Checking"
                                          :credit-account "Groceries"})
          balance (get-balance (d/db conn) "Groceries")]
      (is (= -106M balance))))
  (testing "Crediting a liability account increases the balance"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/description "Kroger"
                                          :transaction/date (now)
                                          :amount 107M
                                          :debit-account "Groceries"
                                          :credit-account "Credit card"})
          balance (get-balance (d/db conn) "Credit card")]
        (is (= 107M balance))))
    (testing "Crediting an equity account increases the balance"
      (let [conn (new-test-db)
            _ (add-simple-transaction conn {:transaction/description "Checking opening balance"
                                            :transaction/date (now)
                                            :amount 108M
                                            :debit-account "Checking"
                                            :credit-account "Opening balances"})
            balance (get-balance (d/db conn) "Opening balances")]
        (is (= 108M balance))))
    (testing "Crediting an income account increases the balance"
      (let [conn (new-test-db)
            _ (add-simple-transaction conn {:transaction/description "Paycheck"
                                            :transaction/date (now)
                                            :amount 109M
                                            :debit-account "Checking"
                                            :credit-account "Salary"})
            balance (get-balance (d/db conn) "Salary")]
        (is (= 109M balance)))))

(deftest transaction-balance-chain
  (testing "The balance for the first transaction item for an account is the same as its polarized amount"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/date #inst "2015-09-01"
                                          :transaction/description "Paycheck"
                                          :amount 1000M
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
          checking (resolve-account (d/db conn) "Checking")
          [transaction-item transaction] (first (get-account-transaction-items (d/db conn) (:db/id checking)))]
      (is (= 1000M (:transaction-item/balance transaction-item)))))
  (testing "The transaction item balance is the sum of the polarized amount for the item and the previous transaction item balance"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/date #inst "2015-09-01"
                                          :transaction/description "Paycheck"
                                          :amount 1000M
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
          _ (add-simple-transaction conn {:transaction/date #inst "2015-09-02"
                                          :transaction/description "Kroger"
                                          :amount 150M
                                          :debit-account "Groceries"
                                          :credit-account "Checking"})
          checking (resolve-account (d/db conn) "Checking")
          [transaction-item transaction] (first (get-account-transaction-items (d/db conn) (:db/id checking)))]
      (is (= 150M (:transaction-item/amount transaction-item)))
      (is (= 850M (:transaction-item/balance transaction-item)))))
  (testing "Adding a transaction before other transactions causes the following transactions' balances to be updated"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/date #inst "2015-09-15"
                                          :transaction/description "Paycheck"
                                          :amount 1000M
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
          _ (add-simple-transaction conn {:transaction/date #inst "2015-09-16"
                                          :transaction/description "Kroger"
                                          :amount 100M
                                          :debit-account "Groceries"
                                          :credit-account "Checking"})
          checking-id (resolve-account-id (d/db conn) "Checking")
          before (get-account-transaction-items (d/db conn) checking-id)
          _ (add-simple-transaction conn {:transaction/date #inst "2015-09-01"
                                          :transaction/description "Paycheck"
                                          :amount 1000M
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
          after (get-account-transaction-items (d/db conn) checking-id)]
      (is (= [[#inst "2015-09-16" 900M]
              [#inst "2015-09-15" 1000M]]
             (map #(vector (-> % second :transaction/date)
                           (-> % first :transaction-item/balance)) before)))
      (is (= [[#inst "2015-09-16" 1900M]
              [#inst "2015-09-15" 2000M]
              [#inst "2015-09-01" 1000M]]
             (map #(vector (-> % second :transaction/date)
                           (-> % first :transaction-item/balance)) after))))))

(deftest transaction-item-indexing
  (testing "adding a transaction with a date after the last item puts the item at the end (front) of the account items with an index one greater than the pervious last"
    (let [conn (new-test-db)
          _ (add-simple-transaction conn {:transaction/date #inst "2015-09-01"
                                          :transaction/description "Paycheck"
                                          :amount 1000M
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
          _ (add-simple-transaction conn {:transaction/date #inst "2015-09-02"
                                          :transaction/description "Kroger"
                                          :amount 100M
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
                                          :amount 100M
                                          :debit-account "Groceries"
                                          :credit-account "Checking"})
          _ (add-simple-transaction conn {:transaction/date #inst "2015-09-01"
                                          :transaction/description "Paycheck"
                                          :amount 1000M
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
          account (resolve-account (d/db conn) "Checking")
          items (get-account-transaction-items (d/db conn) (:db/id account))
          expected [{:transaction-item/index 1
                     :transaction/date #inst "2015-09-02"
                     :transaction-item/balance 900M}
                    {:transaction-item/index 0
                     :transaction/date #inst "2015-09-01"
                     :transaction-item/balance 1000M}]
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
                                          :amount 100M
                                          :debit-account "Groceries"
                                          :credit-account "Checking"})
          _ (add-simple-transaction conn {:transaction/date #inst "2015-09-01"
                                          :transaction/description "Paycheck"
                                          :amount 1000M
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
          account (resolve-account (d/db conn) "Checking")
          items (get-account-transaction-items (d/db conn) (:db/id account))
          expected [{:transaction-item/index 1
                     :transaction/date #inst "2015-09-01"
                     :transaction-item/balance 900M}
                    {:transaction-item/index 0
                     :transaction/date #inst "2015-09-01"
                     :transaction-item/balance 1000M}]
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
                                          :amount 1000M
                                          :credit-account "Salary"
                                          :debit-account "Checking"})
          _ (add-simple-transaction conn {:transaction/date #inst "2015-02-01"
                                          :transaction/description "Paycheck"
                                          :amount 1000M
                                          :credit-account "Salary"
                                          :debit-account "Checking"})
          _ (add-transaction conn {:transaction/date #inst "2015-01-15"
                                   :transaction/description "Paycheck with bonus"
                                   :transaction/items [{:transaction-item/action :transaction-item.action/credit
                                                        :transaction-item/account "Salary"
                                                        :transaction-item/amount 1000M}
                                                       {:transaction-item/action :transaction-item.action/credit
                                                        :transaction-item/account "Salary"
                                                        :transaction-item/amount 500M}
                                                       {:transaction-item/action :transaction-item.action/debit
                                                        :transaction-item/account "Checking"
                                                        :transaction-item/amount 1500M}]})
          salary-id (resolve-account-id (d/db conn) "Salary")
          actual (->> (get-account-transaction-items (d/db conn) salary-id)
                      (map #(merge (first %) (second %)))
                      (map #(select-keys % [:transaction/date :transaction-item/amount :transaction-item/balance])))
          expected [{:transaction/date #inst "2015-02-01"
                     :transaction-item/amount 1000M
                     :transaction-item/balance 3500M}
                    {:transaction/date #inst "2015-01-15"
                     :transaction-item/amount 500M
                     :transaction-item/balance 2500M}
                    {:transaction/date #inst "2015-01-15"
                     :transaction-item/amount 1000M
                     :transaction-item/balance 2000M}
                    {:transaction/date #inst "2015-01-01"
                     :transaction-item/amount 1000M
                     :transaction-item/balance 1000M}]]
      (is (= expected actual)))
    (testing "A transaction with multiple items referencing children of the same account is handled correctly"
      (let [conn (new-test-db)
            _ (add-transaction conn {:transaction/date #inst "2015-01-01"
                                     :transaction/description "Paycheck"
                                     :transaction/items [{:transaction-item/action :transaction-item.action/credit
                                                          :transaction-item/account "Salary"
                                                          :transaction-item/amount 1000M}
                                                         {:transaction-item/action :transaction-item.action/debit
                                                          :transaction-item/account "Taxes/Federal"
                                                          :transaction-item/amount 150M}
                                                         {:transaction-item/action :transaction-item.action/debit
                                                          :transaction-item/account "Taxes/Social security"
                                                          :transaction-item/amount 62M}
                                                         {:transaction-item/action :transaction-item.action/debit
                                                          :transaction-item/account "Taxes/Medicare"
                                                          :transaction-item/amount 14.50M}
                                                         {:transaction-item/action :transaction-item.action/debit
                                                          :transaction-item/account "Checking"
                                                          :transaction-item/amount 773.50M}]})
            children-balance (:account/children-balance (resolve-account (d/db conn) "Taxes"))]
        (is (= 226.50M children-balance))))))

(defn get-account-transaction-item
  [transaction account]
  (->> transaction
       :transaction/items
       (filter #(= (:db/id (:transaction-item/account %)) (:db/id account)))
       first))

(deftest add-an-item
  (testing "When an item is added to an existing transaction, all balances are updated correctly"
    (let [conn (new-test-db)
          ; Add a couple of transactions
          _ (add-simple-transaction conn {:transaction/date #inst "2015-01-01"
                                          :transaction/description "Paycheck"
                                          :amount 1000M
                                          :debit-account "Checking"
                                          :credit-account "Salary"})
          _ (add-simple-transaction conn {:transaction/date #inst "2015-01-04"
                                          :transaction/description "Kroger"
                                          :amount 100M
                                          :debit-account "Groceries"
                                          :credit-account "Checking"})

          ; Update the first by adding an item
          trans (->> (get-transactions (d/db conn))
                     (filter #(= #inst "2015-01-01" (:transaction/date %)))
                     first)
          checking (resolve-account (d/db conn) "Checking")
          salary (resolve-account (d/db conn) "Salary")
          checking-item (get-account-transaction-item trans checking)
          salary-item (get-account-transaction-item trans salary)
          updated-trans (assoc trans :transaction/items [(assoc checking-item :transaction-item/amount 800M)
                                                         {:transaction-item/account "Taxes/Federal"
                                                          :transaction-item/amount 200M
                                                          :transaction-item/action :transaction-item.action/debit}
                                                         salary-item])
          _ (update-transaction conn updated-trans)

          ; check the account balances
          checking-balance (:account/balance (resolve-account (d/db conn) "Checking"))
          salary-balance (:account/balance (resolve-account (d/db conn) "Salary"))
          federal-balance (:account/balance (resolve-account (d/db conn) "Taxes/Federal"))]
      (is (= 700M checking-balance))
      (is (= 1000M salary-balance))
      (is (= 200M federal-balance)))))

; TODO remove-an-item
