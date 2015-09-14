(ns clj-money.reports-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [datomic.api :as d :refer [db]])
  (:use clj-money.test-common
        clj-money.accounts
        clj-money.budgets
        clj-money.transactions
        clj-money.reports))

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
    (add-budget-item conn "2015" "Groceries/Food"     (repeat 12 (bigdec  245)))
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
          expected [{:path "Income"             :budget (bigdec 1800) :value (bigdec 2000) :difference (bigdec  200) :percent-difference (bigdec  0.111) :actual-per-month (bigdec 2000) :depth 0 :style :header}
                    {:path "Salary"             :budget (bigdec 1800) :value (bigdec 2000) :difference (bigdec  200) :percent-difference (bigdec  0.111) :actual-per-month (bigdec 2000) :depth 0 :style :data}
                    {:path "Expense"            :budget  (bigdec 395) :value  (bigdec 300) :difference  (bigdec -95) :percent-difference (bigdec -0.241) :actual-per-month  (bigdec 300) :depth 0 :style :header}
                    {:path "Groceries/Non-food" :budget  (bigdec 150) :value  (bigdec 100) :difference  (bigdec -50) :percent-difference (bigdec -0.333) :actual-per-month  (bigdec 100) :depth 1 :style :data}
                    {:path "Groceries/Food"     :budget  (bigdec 245) :value  (bigdec 200) :difference  (bigdec -45) :percent-difference (bigdec -0.184) :actual-per-month  (bigdec 200) :depth 1 :style :data}]
          report (budget-report (d/db conn) "2015" 1)
          actual (mapv #(select-keys % (-> expected first keys)) report)]
      (is (= expected actual)))))

(deftest create-a-budget-monitor
  (testing "A budget monitor compares actual spending to projected spending with a progress bar kind of approach"
    (let [conn (populate-db)
          report (budget-monitor (d/db conn) "Groceries/Food" #inst "2015-01-20")
          expected {:budget (bigdec 245)
                     :expected (bigdec 158)
                     :expected-percent 20/31
                     :actual (bigdec 200)
                     :actual-percent (bigdec 0.82)
                     :projected (bigdec 310)}]
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
