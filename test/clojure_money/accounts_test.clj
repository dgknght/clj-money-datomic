(ns clojure-money.accounts-test
  (:require [expectations :refer :all]
            [clojure-money.core-test :refer [create-empty-db]]
            [clojure-money.accounts :refer :all]))

;; After I save an account, it appears in the list of all accounts
(expect (more-> "Checking" :account/name
                :account.type/asset :account/type
                (bigdec 0) :account/balance)
        (let [conn (create-empty-db)]
          (add-account conn "Checking")
          (first (all-accounts conn))))

(expect (more-> 2 count
                "Checking" (-> first :account/name)
                "Savings" (-> second :account/name))
        (let [conn (create-empty-db)]
          (add-account conn "Checking")
          (add-account conn "Savings")
          (all-accounts conn)))

;; I can retrieve and account by path (name, prepended by the name of any parents)
(expect (more-> "Checking" :account/name)
        (let [conn (create-empty-db)]
          (add-account conn "Checking")
          (find-account-by-path conn "Checking")))

;; An account can be an asset, liability, equity, income, or expense
(expect (more-> 1 count)
        (let [conn (create-empty-db)]
          (add-account conn "Credit card" :account.type/liability)
          (all-accounts conn)))
(expect Exception
        (let [conn (create-empty-db)]
          (add-account conn "Checking" "notatype")
          (all-accounts conn)))

; Assets and expenses accounts are "left-side" of the equation A = L + E
(expect true
        (let [conn (create-empty-db)]
          (add-account conn "Checking" :account.type/asset)
          (left-side? (find-account-by-path conn "Checking"))))
(expect true
        (let [conn (create-empty-db)]
          (add-account conn "Rent" :account.type/expense)
          (left-side? (find-account-by-path conn "Rent"))))

; Liability, equity, and income accounts are "right-side" of the equation A = L + E
(expect true
        (let [conn (create-empty-db)]
            (add-account conn "Credit card" :account.type/liability)
            (right-side? (find-account-by-path conn "Credit card"))))
(expect true
        (let [conn (create-empty-db)]
            (add-account conn "Opening balances" :account.type/equity)
            (right-side? (find-account-by-path conn "Opening balances"))))
(expect true
        (let [conn (create-empty-db)]
            (add-account conn "Salary" :account.type/income)
            (right-side? (find-account-by-path conn "Salary"))))

;;                  Debit     Credit
;; Asset            Increase  Decrease
;; Liability        Decrease  Increase
;; Income/Revenue   Decrease  Increase
;; Expense          Increase  Decrease
;; Equity/Capital   Decrease  Increase

;; debiting an asset account increases the balance
(expect (bigdec 100)
        (let [conn (create-empty-db)]
          (add-account conn "Checking")
          (let [id (find-account-id-by-path conn "Checking")]
            (debit-account conn id (bigdec 100))
            (get-balance conn "Checking"))))

;; crediting an asset account decreases the balance
(expect (bigdec -100)
        (let [conn (create-empty-db)]
          (add-account conn "Checking")
          (let [id (find-account-id-by-path conn "Checking")]
            (credit-account conn id (bigdec 100))
            (get-balance conn "Checking"))))

;; debiting an expense account increases the balance
(expect (bigdec 100)
        (let [conn (create-empty-db)]
            (add-account conn "Rent" :account.type/expense)
          (let [id (find-account-id-by-path conn "Rent")]
            (debit-account conn id (bigdec 100))
            (get-balance conn "Rent"))))

;; crediting an expense account decreases the balance
(expect (bigdec -100)
        (let [conn (create-empty-db)]
          (add-account conn "Rent" :account.type/expense)
          (let [id (find-account-id-by-path conn "Rent")]
            (credit-account conn id (bigdec 100))
            (get-balance conn "Rent"))))

;; debiting a liability account decreases the balance
(expect (bigdec -100)
        (let [conn (create-empty-db)]
          (add-account conn "Credit card" :account.type/liability)
          (let [id (find-account-id-by-path conn "Credit card")]
            (debit-account conn id (bigdec 100))
            (get-balance conn "Credit card"))))

;; crediting a liability account increases the balance
(expect (bigdec 100)
        (let [conn (create-empty-db)]
          (add-account conn "Credit card" :account.type/liability)
          (let [id (find-account-id-by-path conn "Credit card")]
            (credit-account conn id (bigdec 100))
            (get-balance conn "Credit card"))))

;; debiting an equity account decreases the balance
(expect (bigdec -100)
        (let [conn (create-empty-db)]
          (add-account conn "Opening balances" :account.type/equity)
          (let [id (find-account-id-by-path conn "Opening balances")]
            (debit-account conn id (bigdec 100))
            (get-balance conn "Opening balances"))))

;; crediting an equity account increases the balance
(expect (bigdec 100)
        (let [conn (create-empty-db)]
          (add-account conn "Opening balances" :account.type/equity)
          (let [id (find-account-id-by-path conn "Opening balances")]
            (credit-account conn id (bigdec 100))
            (get-balance conn "Opening balances"))))

;; debiting an income account decreases the balance
(expect (bigdec -100)
        (let [conn (create-empty-db)]
          (add-account conn "Salary" :account.type/income)
          (let [id (find-account-id-by-path conn "Salary")]
            (debit-account conn id (bigdec 100))
            (get-balance conn "Salary"))))

;; crediting an income account increases the balance
(expect (bigdec 100)
        (let [conn (create-empty-db)]
          (add-account conn "Salary" :account.type/income)
          (let [id (find-account-id-by-path conn "Salary")]
            (credit-account conn id (bigdec 100))
            (get-balance conn "Salary"))))
