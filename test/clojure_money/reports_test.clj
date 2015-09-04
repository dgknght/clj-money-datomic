(ns clojure-money.reports-test
  (:require [clojure.test :refer :all]
            [clojure.data :refer [diff]]
            [datomic.api :as d :refer [db]])
  (:use clojure-money.test-common
        clojure-money.accounts
        clojure-money.budgets
        clojure-money.transactions
        clojure-money.reports))

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

    (add-budget conn "2015" #inst "2015-01-01")
    (add-budget-item conn "2015" "Salary"             (repeat 12 (bigdec 1800)))
    (add-budget-item conn "2015" "Groceries/Food"     (repeat 12 (bigdec  250)))
    (add-budget-item conn "2015" "Groceries/Non-food" (repeat 12 (bigdec  150)))

    (add-simple-transaction conn {:transaction/date #inst "2015-01-01"
                                  :transaction/description "Opening balance"
                                  :amount (bigdec 20000)
                                  :debit-account "Savings/Reserve"
                                  :credit-account "Opening balances"})
    (add-simple-transaction conn {:transaction/date #inst "2015-01-01"
                                  :transaction/description "Opening balance"
                                  :amount (bigdec 12000)
                                  :debit-account "Savings/Car"
                                  :credit-account "Opening balances"})
    (add-simple-transaction conn {:transaction/date #inst "2015-01-01"
                                  :transaction/description "Paycheck"
                                  :amount (bigdec 1000)
                                  :debit-account "Checking"
                                  :credit-account "Salary"})
    (add-simple-transaction conn {:transaction/date #inst "2015-01-04"
                                  :transaction/description "Kroger"
                                  :amount (bigdec 100)
                                  :debit-account "Groceries/Food"
                                  :credit-account "Credit card"})
    (add-simple-transaction conn {:transaction/date #inst "2015-01-11"
                                  :transaction/description "Kroger"
                                  :amount (bigdec 100)
                                  :debit-account "Groceries/Food"
                                  :credit-account "Credit card"})
    (add-simple-transaction conn {:transaction/date #inst "2015-01-15"
                                  :transaction/description "Paycheck"
                                  :amount (bigdec 1000)
                                  :debit-account "Checking"
                                  :credit-account "Salary"})
    (add-simple-transaction conn {:transaction/date #inst "2015-01-18"
                                  :transaction/description "Kroger"
                                  :amount (bigdec 100)
                                  :debit-account "Groceries/Non-food"
                                  :credit-account "Credit card"})
  conn))

(deftest create-a-balance-sheet-report
  (testing "The report includes the current balances when no date is specified"
    (let [conn (populate-db)
          report (balance-sheet-report (d/db conn) #inst "2015-01-31")
          actual (mapv #(select-keys % [:caption :value :depth :style]) report)
          expected [{:caption "Assets"            :value (bigdec 34000) :depth 0 :style :header}
                     {:caption "Checking"          :value (bigdec 2000)  :depth 0 :style :data}
                     {:caption "Savings"           :value (bigdec 32000) :depth 0 :style :data}
                     {:caption "Car"               :value (bigdec 12000) :depth 1 :style :data}
                     {:caption "Reserve"           :value (bigdec 20000) :depth 1 :style :data}
                     {:caption "Liabilities"       :value (bigdec 300)   :depth 0 :style :header}
                     {:caption "Credit card"       :value (bigdec 300)   :depth 0 :style :data}
                     {:caption "Equity"            :value (bigdec 33700) :depth 0 :style :header}
                     {:caption "Opening balances"  :value (bigdec 32000) :depth 0 :style :data}
                     {:caption "Retained earnings" :value (bigdec 1700)  :depth 0 :style :data}]]
      (is (= expected actual))))
  (testing "The report includes previous balances when given a date"
    (let [conn (populate-db)
          report (balance-sheet-report (d/db conn) #inst "2015-01-15")
          actual (mapv #(select-keys % [:caption :value :depth :style]) report)
          expected [{:caption "Assets"            :value (bigdec 34000) :depth 0 :style :header}
                     {:caption "Checking"          :value (bigdec 2000)  :depth 0 :style :data}
                     {:caption "Savings"           :value (bigdec 32000) :depth 0 :style :data}
                     {:caption "Car"               :value (bigdec 12000) :depth 1 :style :data}
                     {:caption "Reserve"           :value (bigdec 20000) :depth 1 :style :data}
                     {:caption "Liabilities"       :value (bigdec 200)   :depth 0 :style :header}
                     {:caption "Credit card"       :value (bigdec 200)   :depth 0 :style :data}
                     {:caption "Equity"            :value (bigdec 33800) :depth 0 :style :header}
                     {:caption "Opening balances"  :value (bigdec 32000) :depth 0 :style :data}
                     {:caption "Retained earnings" :value (bigdec 1800)  :depth 0 :style :data}]]
      (is (= expected actual)))))

(deftest create-an-income-statement-report
  (testing "The report includes data between the specified dates"
    (let [conn (populate-db)
          report (income-statement-report (d/db conn) #inst "2015-01-01" #inst "2015-01-31")
          actual (mapv #(select-keys % [:caption :value :depth :style]) report)
          expected [{:caption "Income"    :value (bigdec 2000) :depth 0 :style :header}
                    {:caption "Salary"    :value (bigdec 2000) :depth 0 :style :data}
                    {:caption "Expense"   :value (bigdec 300)  :depth 0 :style :header}
                    {:caption "Groceries" :value (bigdec 300)  :depth 0 :style :data}
                    {:caption "Food"      :value (bigdec 200)  :depth 1 :style :data}
                    {:caption "Non-food"  :value (bigdec 100)  :depth 1 :style :data}] ]
      (is (= expected actual))))
  (testing "The report excludes data not between the specified dates"
    (let [conn (populate-db)
          report (income-statement-report (d/db conn) #inst "2015-01-01" #inst "2015-01-04")
          actual (mapv #(select-keys % [:caption :value :depth :style]) report)
          expected [{:caption "Income"    :value (bigdec 1000) :depth 0 :style :header}
                    {:caption "Salary"    :value (bigdec 1000) :depth 0 :style :data}
                    {:caption "Expense"   :value (bigdec 100)  :depth 0 :style :header}
                    {:caption "Groceries" :value (bigdec 100)  :depth 0 :style :data}
                    {:caption "Food"      :value (bigdec 100)  :depth 1 :style :data}
                    {:caption "Non-food"  :value (bigdec 0)    :depth 1 :style :data}]]
      (is (= expected actual)))))

(deftest create-a-budget-report
  (testing "A budget report compaers budgeted amounts to actual amounts"
    (let [conn (populate-db)
          report (budget-report (d/db conn) "2015" 1)]
      (is (= report [{:caption "Income"    :budget (bigdec 1800) :value (bigdec 2000) :difference (bigdec  200) :percent-difference (bigdec  0.111) :actual-per-month (bigdec 2000) :depth 0 :style :header}
                     {:caption "Salary"    :budget (bigdec 1800) :value (bigdec 2000) :difference (bigdec  200) :percent-difference (bigdec  0.111) :actual-per-month (bigdec 2000) :depth 0 :style :data}
                     {:caption "Expense"   :budget  (bigdec 400) :value  (bigdec 300) :difference (bigdec -100) :percent-difference (bigdec -0.250) :actual-per-month  (bigdec 300) :depth 0 :style :header }
                     {:caption "Groceries" :budget  (bigdec 400) :value  (bigdec 300) :difference (bigdec -100) :percent-difference (bigdec -0.250) :actual-per-month  (bigdec 300) :depth 0 :style :data }
                     {:caption "Food"      :budget  (bigdec 250) :value  (bigdec 200) :difference  (bigdec -50) :percent-difference (bigdec -0.200) :actual-per-month  (bigdec 200) :depth 1 :style :data }
                     {:caption "Non-food"  :budget  (bigdec 150) :value  (bigdec 100) :difference  (bigdec -50) :percent-difference (bigdec -0.333) :actual-per-month  (bigdec 100) :depth 1 :style :data }])))))

(deftest create-a-budget-monitor
  (testing "A budget monitor compares actual spending to projected spending with a progress bar kind of approach"
    (let [conn (populate-db)
          report (budget-monitor (d/db conn) "Groceries/Food" #inst "2015-01-20")]
      (is (= report {:budget (bigdec 250)
                     :expected (bigdec 161)
                     :expected-percent 20/31
                     :actual (bigdec 200)
                     :actual-percent (bigdec 0.8)
                     :projected (bigdec 310)})))))

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
