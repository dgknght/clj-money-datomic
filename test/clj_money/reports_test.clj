(ns clj-money.reports-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [datomic.api :as d :refer [db]])
  (:use clj-money.test-common
        clj-money.util
        clj-money.accounts
        clj-money.budgets
        clj-money.transactions
        clj-money.reports))

(defn budget-item-periods
  [amount]
  (map #(hash-map :budget-item-period/index %
                  :budget-item-period/amount amount)
       (range 0 12)))

(defn populate-db
  "Creates and populates a database"
  []
  (let [conn (create-empty-db)]
    (add-accounts conn [{:account/name "Checking"         :account/type :account.type/asset}
                        {:account/name "Savings"          :account/type :account.type/asset}
                        {:account/name "Car"              :account/type :account.type/asset :account/parent "Savings"}
                        {:account/name "Reserve"          :account/type :account.type/asset :account/parent "Savings"}
                        {:account/name "Credit card"      :account/type :account.type/liability}
                        {:account/name "Opening balances" :account/type :account.type/equity}
                        {:account/name "Salary"           :account/type :account.type/income}
                        {:account/name "Groceries"        :account/type :account.type/expense}
                        {:account/name "Food"             :account/type :account.type/expense :account/parent "Groceries"}
                        {:account/name "Non-food"         :account/type :account.type/expense :account/parent "Groceries"}])


    (try
    (add-budget conn {:budget/name "2015" :budget/start-date #inst "2015-01-01"})
    (catch Exception e
      (clojure.pprint/pprint (ex-data e))
      (throw e)))

    (add-budget-item conn {:budget/_items "2015"
                           :budget-item/account "Salary"
                           :budget-item/periods (budget-item-periods 1800M)})
    (add-budget-item conn {:budget/_items "2015"
                           :budget-item/account "Groceries/Food"
                           :budget-item/periods (budget-item-periods 245M)})
    (add-budget-item conn {:budget/_items "2015"
                           :budget-item/account "Groceries/Non-food"
                           :budget-item/periods (budget-item-periods 150M)})

    (add-simple-transaction conn {:transaction/date #inst "2015-01-01"
                                  :transaction/description "Opening balance"
                                  :amount 20000M
                                  :debit-account "Savings/Reserve"
                                  :credit-account "Opening balances"})
    (add-simple-transaction conn {:transaction/date #inst "2015-01-01"
                                  :transaction/description "Opening balance"
                                  :amount 12000M
                                  :debit-account "Savings/Car"
                                  :credit-account "Opening balances"})
    (add-simple-transaction conn {:transaction/date #inst "2015-01-01"
                                  :transaction/description "Paycheck"
                                  :amount 1000M
                                  :debit-account "Checking"
                                  :credit-account "Salary"})
    (add-simple-transaction conn {:transaction/date #inst "2015-01-04"
                                  :transaction/description "Kroger"
                                  :amount 100M
                                  :debit-account "Groceries/Food"
                                  :credit-account "Credit card"})
    (add-simple-transaction conn {:transaction/date #inst "2015-01-11"
                                  :transaction/description "Kroger"
                                  :amount 100M
                                  :debit-account "Groceries/Food"
                                  :credit-account "Credit card"})
    (add-simple-transaction conn {:transaction/date #inst "2015-01-15"
                                  :transaction/description "Paycheck"
                                  :amount 1000M
                                  :debit-account "Checking"
                                  :credit-account "Salary"})
    (add-simple-transaction conn {:transaction/date #inst "2015-01-18"
                                  :transaction/description "Kroger"
                                  :amount 100M
                                  :debit-account "Groceries/Non-food"
                                  :credit-account "Credit card"})
  conn))

(deftest create-a-balance-sheet-report
  (testing "The report includes the current balances when no date is specified"
    (let [conn (populate-db)
          report (balance-sheet-report (d/db conn) #inst "2015-01-31")
          actual (mapv #(select-keys % [:caption :value :depth :style]) report)
          expected [{:caption "Assets"            :value 34000M :depth 0 :style :header}
                    {:caption "Checking"          :value 2000M  :depth 0 :style :data}
                    {:caption "Savings"           :value 32000M :depth 0 :style :data}
                    {:caption "Car"               :value 12000M :depth 1 :style :data}
                    {:caption "Reserve"           :value 20000M :depth 1 :style :data}
                    {:caption "Liabilities"       :value 300M   :depth 0 :style :header}
                    {:caption "Credit card"       :value 300M   :depth 0 :style :data}
                    {:caption "Equity"            :value 33700M :depth 0 :style :header}
                    {:caption "Opening balances"  :value 32000M :depth 0 :style :data}
                    {:caption "Retained earnings" :value 1700M  :depth 0 :style :data}]]
      (is (= expected actual))))
  (testing "The report includes previous balances when given a date"
    (let [conn (populate-db)
          report (balance-sheet-report (d/db conn) #inst "2015-01-15")
          actual (mapv #(select-keys % [:caption :value :depth :style]) report)
          expected [{:caption "Assets"            :value 34000M :depth 0 :style :header}
                    {:caption "Checking"          :value 2000M  :depth 0 :style :data}
                    {:caption "Savings"           :value 32000M :depth 0 :style :data}
                    {:caption "Car"               :value 12000M :depth 1 :style :data}
                    {:caption "Reserve"           :value 20000M :depth 1 :style :data}
                    {:caption "Liabilities"       :value 200M   :depth 0 :style :header}
                    {:caption "Credit card"       :value 200M   :depth 0 :style :data}
                    {:caption "Equity"            :value 33800M :depth 0 :style :header}
                    {:caption "Opening balances"  :value 32000M :depth 0 :style :data}
                    {:caption "Retained earnings" :value 1800M  :depth 0 :style :data}]]
      (is (= expected actual)))))

(deftest create-an-income-statement-report
  (testing "The report includes data between the specified dates"
    (let [conn (populate-db)
          report (income-statement-report (d/db conn) #inst "2015-01-01" #inst "2015-01-31")
          actual (mapv #(select-keys % [:caption :value :depth :style]) report)
          expected [{:caption "Income"    :value 2000M :depth 0 :style :header}
                    {:caption "Salary"    :value 2000M :depth 0 :style :data}
                    {:caption "Expense"   :value 300M  :depth 0 :style :header}
                    {:caption "Groceries" :value 300M  :depth 0 :style :data}
                    {:caption "Food"      :value 200M  :depth 1 :style :data}
                    {:caption "Non-food"  :value 100M  :depth 1 :style :data}] ]
      (is (= expected actual))))
  (testing "The report excludes data not between the specified dates"
    (let [conn (populate-db)
          report (income-statement-report (d/db conn) #inst "2015-01-01" #inst "2015-01-04")
          actual (mapv #(select-keys % [:caption :value :depth :style]) report)
          expected [{:caption "Income"    :value 1000M :depth 0 :style :header}
                    {:caption "Salary"    :value 1000M :depth 0 :style :data}
                    {:caption "Expense"   :value 100M  :depth 0 :style :header}
                    {:caption "Groceries" :value 100M  :depth 0 :style :data}
                    {:caption "Food"      :value 100M  :depth 1 :style :data}
                    {:caption "Non-food"  :value 0M    :depth 1 :style :data}]]
      (is (= expected actual)))))

(defn report-diff
  [expected actual]
  (dorun (map-indexed (fn [index e]
                        (let [a (get actual index)
                              d (clojure.data/diff e a)]
                          (when (first d)
                            (println "expected: " (prn-str e))
                            (println "actual: " (prn-str a))
                            (println "diff: " (prn-str d))
                            (println "-------------------------------"))))
                      expected)))

(deftest create-a-budget-report
  (testing "A budget report compaers budgeted amounts to actual amounts"
    (let [conn (populate-db)
          expected [{:path "Income"             :budget 1800M :value 2000M :difference  200M :percent-difference  0.111M :actual-per-month 2000M :depth 0 :style :header}
                    {:path "Salary"             :budget 1800M :value 2000M :difference  200M :percent-difference  0.111M :actual-per-month 2000M :depth 0 :style :data}
                    {:path "Expense"            :budget  395M :value  300M :difference  -95M :percent-difference -0.241M :actual-per-month  300M :depth 0 :style :header}
                    {:path "Groceries/Non-food" :budget  150M :value  100M :difference  -50M :percent-difference -0.333M :actual-per-month  100M :depth 1 :style :data}
                    {:path "Groceries/Food"     :budget  245M :value  200M :difference  -45M :percent-difference -0.184M :actual-per-month  200M :depth 1 :style :data}]
          report (budget-report (d/db conn) "2015" 1)
          actual (mapv #(select-keys % (-> expected first keys)) report)]
      (is (= expected actual)))))

(deftest create-a-budget-monitor
  (testing "A budget monitor compares actual spending to projected spending with a progress bar kind of approach"
    (let [conn (populate-db)
          report (budget-monitor (d/db conn) "Groceries/Food" #inst "2015-01-20")
          expected {:budget 245M
                     :expected 158M
                     :expected-percent 20/31
                     :actual 200M
                     :actual-percent 0.82M
                     :projected 310M}]
      (is (= expected report)))))

(deftest create-display-records
  (testing "Display records can be created from existing accounts"
    (let [conn (create-empty-db)
          _ (add-accounts conn ["Checking"
                                "Savings"
                                {:account/name "Reserve" :account/parent "Savings"}
                                {:account/name "Salary" :account/type :account.type/income}])
          display-records (display-records (d/db conn))
          actual (map #(select-keys % [:caption :depth :path]) display-records)
          expected [{:caption "Checking" :path "Checking"        :depth 0}
                    {:caption "Salary"   :path "Salary"          :depth 0}
                    {:caption "Savings"  :path "Savings"         :depth 0}
                    {:caption "Reserve"  :path "Savings/Reserve" :depth 1}]]
      (is (= expected actual)))))

(deftest get-an-account-list-with-headers
  (testing "I can get a list of accounts grouped by account type"
    (let [conn (populate-db)
          actual (->> (account-list-with-headers (d/db conn))
                      (mapv #(select-keys % [:caption :style :depth])))
          expected [{:caption "Assets"           :style :header :depth 0}
                    {:caption "Checking"         :style :data   :depth 0}
                    {:caption "Savings"          :style :data   :depth 0}
                    {:caption "Car"              :style :data   :depth 1}
                    {:caption "Reserve"          :style :data   :depth 1}
                    {:caption "Liabilities"      :style :header :depth 0}
                    {:caption "Credit card"      :style :data   :depth 0}
                    {:caption "Equity"           :style :header :depth 0}
                    {:caption "Opening balances" :style :data   :depth 0}
                    {:caption "Income"           :style :header :depth 0}
                    {:caption "Salary"           :style :data   :depth 0}
                    {:caption "Expense"          :style :header :depth 0}
                    {:caption "Groceries"        :style :data   :depth 0}
                    {:caption "Food"             :style :data   :depth 1}
                    {:caption "Non-food"         :style :data   :depth 1}]]
      (is (= expected actual)))))
