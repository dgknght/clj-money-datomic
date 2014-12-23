(ns clojure-money.accounts-test
  (:require [expectations :refer :all]
            [clojure-money.core :refer [conn]]
            [clojure-money.accounts :refer :all]
            [clojure-money.core-test :refer [create-empty-db]]))

;; After I save an account, it appears in the list of all accounts
(expect (more-> "Checking" :account/name
                :account.type/asset :account/type
                (bigdec 0) :account/balance)
        (with-redefs [conn (create-empty-db)]
          (do
            (add-account "Checking")
            (first (all-accounts)))))

(expect (more-> 2 count
                "Checking" (-> first :account/name)
                "Savings" (-> second :account/name))
        (with-redefs [conn (create-empty-db)]
          (do
            (add-account "Checking")
            (add-account "Savings")
            (all-accounts))))

;; An account can be an asset, liability, equity, income, or expense
(expect (more-> 1 count)
        (with-redefs [conn (create-empty-db)]
          (do
            (add-account "Credit card" :account.type/liability)
            (all-accounts))))
(expect Exception
        (with-redefs [conn (create-empty-db)]
          (do
            (add-account "Checking" "notatype")
            (all-accounts))))

;;                  Debit     Credit
;; Asset            Increase  Decrease
;; Liability        Decrease  Increase
;; Income/Revenue   Decrease  Increase
;; Expense          Increase  Decrease
;; Equity/Capital   Decrease  Increase

;; debiting an asset account increases the balance
(expect (bigdec 100)
        (with-redefs [conn (create-empty-db)]
          (do
              (add-account "Checking")
              (debit-account "Checking" (bigdec 100))
              (get-balance "Checking"))))

;; crediting an asset account decreases the balance
(expect (bigdec -100)
        (with-redefs [conn (create-empty-db)]
          (do
              (add-account "Checking")
              (credit-account "Checking" (bigdec 100))
              (get-balance "Checking"))))

;; debiting an expense account increases the balance
(expect (bigdec 100)
        (with-redefs [conn (create-empty-db)]
          (do
              (add-account "Rent" :account.type/expense)
              (debit-account "Rent" (bigdec 100))
              (get-balance "Rent"))))

;; crediting an expense account decreases the balance
(expect (bigdec -100)
        (with-redefs [conn (create-empty-db)]
          (do
              (add-account "Rent" :account.type/expense)
              (credit-account "Rent" (bigdec 100))
              (get-balance "Rent"))))
