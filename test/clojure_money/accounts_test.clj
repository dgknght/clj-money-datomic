(ns clojure-money.accounts-test
  (:require [datomic.api :as d :refer [db]]
            [clojure.test :refer :all]
            [clojure.tools.logging :as log]
            [clojure-money.accounts :refer :all]
            [clojure-money.common :refer :all])
  (:use clojure-money.test-common))

(deftest save-an-account
  (testing "After I save an account, it appears in the list of all accounts"
    (let [conn (create-empty-db)
          _ (add-account conn "Checking")
          accounts (all-accounts (d/db conn))]
      (is (= 1 (count accounts)))
      (is (= "Checking" (-> accounts first :account/name)))))
  (testing "After I save two accounts, they are both in the list of all accounts"
    (let [conn (create-empty-db)
          _ (add-accounts conn ["Checking" "Savings"])
          accounts (all-accounts (d/db conn))]
      (is (= 2 (count accounts)))
      (is (= "Checking" (-> accounts first :account/name)))
      (is (= "Savings" (-> accounts second :account/name))))))

(deftest get-an-account-by-path
  (testing "After I save an account, I can retrieve it by path (name prepened by parents' names)"
    (let [conn (create-empty-db)
          _ (add-account conn "Checking")
          account (find-account-by-path (d/db conn) "Checking")]
      (is (= "Checking" (:account/name account))))))

(deftest get-an-account-id-by-path
  (testing "After I save an account, I can get its ID by path"
    (let [conn (create-empty-db)
          _ (add-account conn "Checking")
          account-id (find-account-id-by-path (d/db conn) "Checking")]
      (is (number? account-id)))))

(deftest an-account-must-have-a-valid-type
  (testing "An account can be created with a valid type"
    (let [conn (create-empty-db)
          _ (add-account conn {:account/name "Credit card" :account/type :account.type/liability})
          accounts (all-accounts (d/db conn))]
      (is (= 1 (count accounts)))))
  (testing "An account cannot be created in an invalid type"
    (let [conn (create-empty-db)]
      (is (thrown-with-msg? Exception #"The account information supplied is not valid."
                            (add-account conn {:account/name "Checking" :account/type "notatype"}))))))

(deftest validations-test
  (testing "Name is a required field"
    (let [conn (create-empty-db)]
      (is (thrown-with-msg? Exception #"The account information supplied is not valid."
                            (add-account conn {})))))
  (testing "An account cannot have a parent with a different type"
    (let [conn (create-empty-db)
          checking (add-account conn "Checking")]
      (is (thrown-with-msg? Exception #"The account information supplied is not valid."
                            (add-account conn {:account/name "Rent"
                                               :account/type :account.type/expense
                                               :account/parent "Checking"}))))))

(deftest calculate-path-test
  (testing "Given an account and a list of accounts containing the parents, a path can be calculated"
    (let [conn (create-empty-db)
          _ (add-account conn {:account/name "Savings" :account/type :account.type/asset})
          _ (add-account conn {:account/name "Reserve" :account/type :account.type/asset :account/parent "Savings"})
          accounts (all-accounts (d/db conn))
          paths (into #{} (map #(calculate-path-with-list % accounts) accounts))]
      (is (= #{"Savings" "Savings/Reserve"} paths)))))

(defn create-and-retrieve-account
  ([account-name account-type]
   (let [conn (create-empty-db)]
     (create-and-retrieve-account conn account-name account-type)))
  ([conn account-name account-type]
   (add-account conn {:account/name account-name :account/type account-type})
   (find-account-by-path (d/db conn) account-name)))

(deftest an-account-is-left-side-or-right-side
  (testing "Asset accounts are 'left-side' of the equestion A = L + E"
    (let [account (create-and-retrieve-account "Checking" :account.type/asset)]
      (is (left-side? account))))
  (testing "Expense accounts are 'left-side' of the equestion A = L + E"
    (let [account (create-and-retrieve-account "Rent" :account.type/expense)]
      (is (left-side? account))))
  (testing "Liability accounts are 'right-side' of the equestion A = L + E"
    (let [account (create-and-retrieve-account "Credit card" :account.type/liability)]
      (is (right-side? account))))
  (testing "Equity accounts are 'right-side' of the equestion A = L + E"
    (let [account (create-and-retrieve-account "Opening balances" :account.type/equity)]
      (is (right-side? account))))
  (testing "Income accounts are 'right-side' of the equestion A = L + E"
    (let [account (create-and-retrieve-account "Salary" :account.type/income)]
      (is (right-side? account)))))

;;                  Debit     Credit
;; Asset            Increase  Decrease
;; Liability        Decrease  Increase
;; Income/Revenue   Decrease  Increase
;; Expense          Increase  Decrease
;; Equity/Capital   Decrease  Increase

(deftest debit-an-account
  (testing "Debiting an asset account increases the balance"
    (let [conn (create-empty-db)
          _ (add-account conn "Checking")
          id (find-account-id-by-path (d/db conn) "Checking")
          _ (debit-account conn id (bigdec 100))
          _ (debit-account conn id (bigdec 100))
          balance (get-balance (d/db conn) id)]
      (is (= (bigdec 200) balance))))
  (testing "Debiting an expense account increases the balance"
    (let [conn (create-empty-db)
          _ (add-account conn {:account/name "Rent" :account/type :account.type/expense})
          id (find-account-id-by-path (d/db conn) "Rent")
          _ (debit-account conn id (bigdec 101))
          _ (debit-account conn id (bigdec 101))
          balance (get-balance (d/db conn) id)]
      (is (= (bigdec 202) balance))))
  (testing "Debiting a liability account decreases the balance"
    (let [conn (create-empty-db)
          _ (add-account conn {:account/name "Credit card" :account/type :account.type/liability})
          id (find-account-id-by-path (d/db conn) "Credit card")
          _ (debit-account conn id (bigdec 102))
          balance (get-balance (d/db conn) id)]
      (is (= (bigdec -102) balance))))
  (testing "Debiting an equity account decreases the balance"
    (let [conn (create-empty-db)
          _ (add-account conn {:account/name "Opening balances" :account/type :account.type/equity})
          id (find-account-id-by-path (d/db conn) "Opening balances")
          _ (debit-account conn id (bigdec 103))
          balance (get-balance (d/db conn) id)]
      (is (= (bigdec -103) balance))))
  (testing "Debiting an income account decreases the balance"
    (let [conn (create-empty-db)
          _ (add-account conn {:account/name "Salary" :account/type :account.type/income})
          id (find-account-id-by-path (d/db conn) "Salary")
          _ (debit-account conn id (bigdec 104))
          balance (get-balance (d/db conn) id)]
      (is (= (bigdec -104) balance)))))

(deftest credit-an-account
  (testing "Crediting an asset account decreases the balance"
    (let [conn (create-empty-db)
          _ (add-account conn "Checking")
          id (find-account-id-by-path (d/db conn) "Checking")
          _ (credit-account conn id (bigdec 100))
          balance (get-balance (d/db conn) id)]
      (is (= (bigdec -100) balance))))
  (testing "Crediting an expense account decreases the balance"
    (let [conn (create-empty-db)
          _ (add-account conn {:account/name "Rent" :account/type :account.type/expense})
          id (find-account-id-by-path (d/db conn) "Rent")
          _ (credit-account conn id (bigdec 111))
          balance (get-balance (d/db conn) id)]
      (is (= (bigdec -111) balance))))
  (testing "Crediting a liability account increases the balance"
    (let [conn (create-empty-db)
          _ (add-account conn {:account/name "Credit card" :account/type :account.type/liability})
          id (find-account-id-by-path (d/db conn) "Credit card")
          _ (credit-account conn id (bigdec 112))
          balance (get-balance (d/db conn) id)]
      (is (= (bigdec 112) balance))))
  (testing "Crediting an equity account increases the balance"
    (let [conn (create-empty-db)
          _ (add-account conn {:account/name "Opening balances" :account/type :account.type/equity})
          id (find-account-id-by-path (d/db conn) "Opening balances")
          _ (credit-account conn id (bigdec 113))
          balance (get-balance (d/db conn) id)]
      (is (= (bigdec 113) balance))))
  (testing "Crediting an income account increases the balance"
    (let [conn (create-empty-db)
          _ (add-account conn {:account/name "Salary" :account/type :account.type/income})
          id (find-account-id-by-path (d/db conn) "Salary")
          _ (credit-account conn id (bigdec 114))
          balance (get-balance (d/db conn) id)]
      (is (= (bigdec 114) balance)))))

(deftest add-a-child-account
  (testing "A a child account can be accessed via its parent"
    (let [conn (create-empty-db)
          _ (add-account conn {:account/name "Savings" :account/type :account.type/asset})
          _ (add-account conn {:account/name "Car" :account/type :account.type/asset :account/parent "Savings"})
          children (child-accounts (d/db conn) "Savings")]
      (is (= 1 (count children)))
      (is (= "Car") (-> children first :account/name))))
  (testing "A child can be retrieved directly by path"
    (let [conn (create-empty-db)
          _ (add-account conn {:account/name "Savings" :account/type :account.type/asset})
          _ (add-account conn {:account/name "Car" :account/type :account.type/asset :account/parent "Savings"})
          child (find-account-by-path (d/db conn) "Savings/Car")]
      (is (= "Car") (:account/name child)))))

(deftest get-root-accounts
  (testing "Root accounts can be retrieved together"
    (let [conn (create-empty-db)
          _ (add-account conn {:account/name "Savings" :account/type :account.type/asset})
          _ (add-account conn {:account/name "Car" :account/type :account.type/asset :account/parent "Savings"})
          _ (add-account conn {:account/name "Reserve" :account/type :account.type/asset :account/parent "Savings"})
          _ (add-account conn {:account/name "Opening Balances" :account/type :account.type/equity})
          root-accounts (->> conn
                             d/db
                             root-accounts
                             (map :account/name)
                             set
                             )]
      (= #{"Savings" "Opening Balances"} root-accounts))))

(deftest get-stacked-accounts
  (testing "Accounts can be retrieved such that children accounts are contained by their parents"
    (let [conn (create-empty-db)
          _ (add-account conn {:account/name "Savings" :account/type :account.type/asset})
          _ (add-account conn {:account/name "Car" :account/type :account.type/asset :account/parent "Savings"})
          _ (add-account conn {:account/name "Reserve" :account/type :account.type/asset :account/parent "Savings"})
          stacked (stacked-accounts (d/db conn))]
      (is (= "Savings" (-> stacked first :account/name)))
      (is (= "Car" (-> stacked first :children first :account/name)))
      (is (= "Reserve" (-> stacked first :children second :account/name))))))

(deftest delete-an-account
  (testing "I can delete an account"
    (let [conn (create-empty-db)
          _ (add-account conn "Checking" :account.type/asset)
          after-add (find-account-by-path (d/db conn) "Checking")
          _ (delete-account conn (:db/id after-add))
          after-delete (find-account-by-path (d/db conn) "Checking")]
      (is (not (nil? after-add)))
      (is (nil? after-delete)))))
