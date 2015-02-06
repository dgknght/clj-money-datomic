(ns clojure-money.accounts-test
  (:require [expectations :refer :all]
            [datomic.api :as d :refer [db]]
            [clojure-money.core-test :refer [create-empty-db]]
            [clojure-money.accounts :refer :all]))

;; After I save an account, it appears in the list of all accounts
(expect (more-> "Checking" :account/name
                :account.type/asset :account/type
                (bigdec 0) :account/balance)
        (let [conn (create-empty-db)]
          (add-account conn "Checking")
          (first (all-accounts (d/db conn)))))

(expect (more-> 2 count
                "Checking" (-> first :account/name)
                "Savings" (-> second :account/name))
        (let [conn (create-empty-db)]
          (add-account conn "Checking")
          (add-account conn "Savings")
          (all-accounts (d/db conn))))

;; I can retrieve and account by path (name, prepended by the name of any parents)
(expect (more-> "Checking" :account/name)
        (let [conn (create-empty-db)]
          (add-account conn "Checking")
          (find-account-by-path (d/db conn) "Checking")))

;; An account can be an asset, liability, equity, income, or expense
(expect (more-> 1 count)
        (let [conn (create-empty-db)]
          (add-account conn "Credit card" :account.type/liability)
          (all-accounts (d/db conn))))
(expect Exception
        (let [conn (create-empty-db)]
          (add-account conn "Checking" "notatype")
          (all-accounts (d/db conn))))

; Assets and expenses accounts are "left-side" of the equation A = L + E
(expect true
        (let [conn (create-empty-db)]
          (add-account conn "Checking" :account.type/asset)
          (left-side? (find-account-by-path (d/db conn) "Checking"))))
(expect true
        (let [conn (create-empty-db)]
          (add-account conn "Rent" :account.type/expense)
          (left-side? (find-account-by-path (d/db conn) "Rent"))))

; Liability, equity, and income accounts are "right-side" of the equation A = L + E
(expect true
        (let [conn (create-empty-db)]
            (add-account conn "Credit card" :account.type/liability)
            (right-side? (find-account-by-path (d/db conn) "Credit card"))))
(expect true
        (let [conn (create-empty-db)]
            (add-account conn "Opening balances" :account.type/equity)
            (right-side? (find-account-by-path (d/db conn) "Opening balances"))))
(expect true
        (let [conn (create-empty-db)]
            (add-account conn "Salary" :account.type/income)
            (right-side? (find-account-by-path (d/db conn) "Salary"))))

;;                  Debit     Credit
;; Asset            Increase  Decrease
;; Liability        Decrease  Increase
;; Income/Revenue   Decrease  Increase
;; Expense          Increase  Decrease
;; Equity/Capital   Decrease  Increase

;; debiting an asset account increases the balance
(expect (bigdec 200)
        (let [conn (create-empty-db)]
          (add-account conn "Checking")
          (let [id (find-account-id-by-path (d/db conn) "Checking")]
            (debit-account conn id (bigdec 100))
            (debit-account conn id (bigdec 100))
            (get-balance (d/db conn) id))))

;; crediting an asset account decreases the balance
(expect (bigdec 50)
        (let [conn (create-empty-db)]
          (add-account conn "Checking")
          (let [id (find-account-id-by-path (d/db conn) "Checking")]
            (debit-account conn id (bigdec 100))
            (credit-account conn id (bigdec 50))
            (get-balance (d/db conn) id))))

;; debiting an expense account increases the balance
(expect (bigdec 200)
        (let [conn (create-empty-db)]
            (add-account conn "Rent" :account.type/expense)
          (let [id (find-account-id-by-path (d/db conn) "Rent")]
            (debit-account conn id (bigdec 100))
            (debit-account conn id (bigdec 100))
            (get-balance (d/db conn) id))))

;; crediting an expense account decreases the balance
(expect (bigdec 50)
        (let [conn (create-empty-db)]
          (add-account conn "Rent" :account.type/expense)
          (let [id (find-account-id-by-path (d/db conn) "Rent")]
            (debit-account conn id (bigdec 100))
            (credit-account conn id (bigdec 50))
            (get-balance (d/db conn) id))))

;; debiting a liability account decreases the balance
(expect (bigdec -100)
        (let [conn (create-empty-db)]
          (add-account conn "Credit card" :account.type/liability)
          (let [id (find-account-id-by-path (d/db conn) "Credit card")]
            (debit-account conn id (bigdec 100))
            (get-balance (d/db conn) id))))

;; crediting a liability account increases the balance
(expect (bigdec 100)
        (let [conn (create-empty-db)]
          (add-account conn "Credit card" :account.type/liability)
          (let [id (find-account-id-by-path (d/db conn) "Credit card")]
            (credit-account conn id (bigdec 100))
            (get-balance (d/db conn) id))))

;; debiting an equity account decreases the balance
(expect (bigdec -100)
        (let [conn (create-empty-db)]
          (add-account conn "Opening balances" :account.type/equity)
          (let [id (find-account-id-by-path (d/db conn) "Opening balances")]
            (debit-account conn id (bigdec 100))
            (get-balance (d/db conn) id))))

;; crediting an equity account increases the balance
(expect (bigdec 100)
        (let [conn (create-empty-db)]
          (add-account conn "Opening balances" :account.type/equity)
          (let [id (find-account-id-by-path (d/db conn) "Opening balances")]
            (credit-account conn id (bigdec 100))
            (get-balance (d/db conn) id))))

;; debiting an income account decreases the balance
(expect (bigdec -100)
        (let [conn (create-empty-db)]
          (add-account conn "Salary" :account.type/income)
          (let [id (find-account-id-by-path (d/db conn) "Salary")]
            (debit-account conn id (bigdec 100))
            (get-balance (d/db conn) id))))

;; crediting an income account increases the balance
(expect (bigdec 100)
        (let [conn (create-empty-db)]
          (add-account conn "Salary" :account.type/income)
          (let [id (find-account-id-by-path (d/db conn) "Salary")]
            (credit-account conn id (bigdec 100))
            (get-balance (d/db conn) id))))

;; I can add a child account to an existing account
(expect ["Car"]
        (let [conn (create-empty-db)]
          (add-account conn "Savings" :account.type/asset)
          (add-account conn "Car", :account.type/asset, "Savings")
          (map :account/name (child-accounts (d/db conn) "Savings"))))

;; I can find a child account directly with its path

;; I can get a list of all root accounts
