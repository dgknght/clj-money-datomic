(ns clojure-money.reports-test
  (:require [expectations :refer :all]
            [datomic.api :as d :refer [db]])
  (:use clojure-money.test-common
        clojure-money.accounts
        clojure-money.transactions
        clojure-money.reports))

(defn populate-db
  "Creates and populates a database"
  []
  (let [conn (create-empty-db)]
    (add-account conn "Checking" :account.type/asset)
    (add-account conn "Savings" :account.type/asset)
    (add-account conn "Car" :account.type/asset "Savings")
    (add-account conn "Reserve" :account.type/asset "Savings")
    (add-account conn "Credit card" :account.type/liability)
    (add-account conn "Opening balances" :account.type/equity)
    (add-account conn "Salary" :account.type/income)
    (add-account conn "Groceries" :account.type/expense)
    (add-account conn "Food" :account.type/expense "Groceries")
    (add-account conn "Non-food" :account.type/expense "Groceries")

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

; Uses the current balances
(expect [{:caption "Assets"            :value (bigdec 34000) :depth 0 :style :header}
         {:caption "Checking"          :value (bigdec 2000)  :depth 0 :style :data}
         {:caption "Savings"           :value (bigdec 32000) :depth 0 :style :data}
         {:caption "Car"               :value (bigdec 12000) :depth 1 :style :data}
         {:caption "Reserve"           :value (bigdec 20000) :depth 1 :style :data}
         {:caption "Liabilities"       :value (bigdec 300)   :depth 0 :style :header}
         {:caption "Credit card"       :value (bigdec 300)   :depth 0 :style :data}
         {:caption "Equity"            :value (bigdec 33700) :depth 0 :style :header}
         {:caption "Opening balances"  :value (bigdec 32000) :depth 0 :style :data}
         {:caption "Retained earnings" :value (bigdec 1700)  :depth 0 :style :data}]
        (let [conn (populate-db)]
          (balance-sheet-report (d/db conn) #inst "2015-01-31")))

; Uses balances as of a prior date
(expect [{:caption "Assets"            :value (bigdec 34000) :depth 0 :style :header}
         {:caption "Checking"          :value (bigdec 2000)  :depth 0 :style :data}
         {:caption "Savings"           :value (bigdec 32000) :depth 0 :style :data}
         {:caption "Car"               :value (bigdec 12000) :depth 1 :style :data}
         {:caption "Reserve"           :value (bigdec 20000) :depth 1 :style :data}
         {:caption "Liabilities"       :value (bigdec 200)   :depth 0 :style :header}
         {:caption "Credit card"       :value (bigdec 200)   :depth 0 :style :data}
         {:caption "Equity"            :value (bigdec 33800) :depth 0 :style :header}
         {:caption "Opening balances"  :value (bigdec 32000) :depth 0 :style :data}
         {:caption "Retained earnings" :value (bigdec 1800)  :depth 0 :style :data}]
        (let [conn (populate-db)]
          (balance-sheet-report (d/db conn) #inst "2015-01-15")))

(expect [{:caption "Income"    :value (bigdec 2000) :depth 0 :style :header}
         {:caption "Salary"    :value (bigdec 2000) :depth 0 :style :data}
         {:caption "Expense"   :value (bigdec 300)  :depth 0 :style :header}
         {:caption "Groceries" :value (bigdec 300)  :depth 0 :style :data}
         {:caption "Food"      :value (bigdec 200)  :depth 1 :style :data}
         {:caption "Non-food"  :value (bigdec 100)  :depth 1 :style :data}]
        (let [conn (populate-db)]
          (income-statement-report (d/db conn) #inst "2015-01-01" #inst "2015-01-31")))

(expect [{:caption "Income"    :value (bigdec 1000) :depth 0 :style :header}
         {:caption "Salary"    :value (bigdec 1000) :depth 0 :style :data}
         {:caption "Expense"   :value (bigdec 100)  :depth 0 :style :header}
         {:caption "Groceries" :value (bigdec 100)  :depth 0 :style :data}
         {:caption "Food"      :value (bigdec 100)  :depth 1 :style :data}
         {:caption "Non-food"  :value (bigdec 0)    :depth 1 :style :data}] 
        (let [conn (populate-db)]
          (income-statement-report (d/db conn) #inst "2015-01-01" #inst "2015-01-04")))
