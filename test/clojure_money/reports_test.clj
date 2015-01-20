(ns clojure-money.reports-test
  (:require [expectations :refer :all]
            [clojure-money.core-test :refer [create-empty-db]]
            [clojure-money.accounts :refer :all]
            [clojure-money.transactions :refer :all]
            [clojure-money.reports :refer :all]))

(defn populate-db
  "Creates and populates a database"
  []
  (let [conn (create-empty-db)]
    (add-account conn "Checking" :account.type/asset)
    (add-account conn "Savings" :account.type/asset)
    (add-account conn "Credit card" :account.type/liability)
    (add-account conn "Opening balances" :account.type/equity)
    (add-account conn "Salary" :account.type/income)
    (add-account conn "Groceries" :account.type/expense)

    (add-simple-transaction conn {:transaction/date #datetime "2015-01-01"
                                  :transaction/description "Opening balance"
                                  :amount (bigdec 1000)
                                  :debit-account "Savings"
                                  :credit-account "Opening balances"})
    (add-simple-transaction conn {:transaction/date #datetime "2015-01-01"
                                  :transaction/description "Paycheck"
                                  :amount (bigdec 1000)
                                  :debit-account "Checking"
                                  :credit-account "Salary"})
    (add-simple-transaction conn {:transaction/date #datetime "2015-01-04"
                                  :transaction/description "Kroger"
                                  :amount (bigdec 100)
                                  :debit-account "Groceries"
                                  :credit-account "Credit card"})
    (add-simple-transaction conn {:transaction/date #datetime "2015-01-11"
                                  :transaction/description "Kroger"
                                  :amount (bigdec 100)
                                  :debit-account "Groceries"
                                  :credit-account "Credit card"})
    (add-simple-transaction conn {:transaction/date #datetime "2015-01-15"
                                  :transaction/description "Paycheck"
                                  :amount (bigdec 1000)
                                  :debit-account "Checking"
                                  :credit-account "Salary"})
    (add-simple-transaction conn {:transaction/date #datetime "2015-01-18"
                                  :transaction/description "Kroger"
                                  :amount (bigdec 100)
                                  :debit-account "Groceries"
                                  :credit-account "Credit card"})))

(expect [{:caption "Assets" :value 22000 :depth 0 :style :header}
         {:caption "Checking" :value 2000 :depth 0 :style :data}
         {:caption "Savings" :value 20000 :depth 0 :style :data}
         {:caption "Liabilities" :value 300 :depth 0 :style :header}
         {:caption "Credit card" :value 300 :depth 0 :style :data}
         {:caption "Equity" :value 21700 :depth 0 :style :header}
         {:caption "Opening balances" :value 20000 :depth 0 :style :data}
         {:caption "Retained earnings" :value 1700 :depth 0 :style :data}]
        (let [db (populate-db)]
          (balance-sheet-report db, #datetime "2015-01-31")))
