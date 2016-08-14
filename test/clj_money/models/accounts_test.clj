(ns clj-money.models.accounts-test
  (:require [datomic.api :as d :refer [db]]
            [clojure.test :refer :all]
            [clojure.tools.logging :as log]
            [clj-money.models.accounts :refer :all]
            [clj-money.common :refer :all]
            [clj-money.test-common :refer [create-empty-db]])
  (:use clj-money.test-common))

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

(deftest get-an-account-and-all-its-parents
  (testing "Given a path, I can get the account with all of its parents"
    (let [conn (create-empty-db)
          _ (add-account conn "Savings")
          _ (add-account conn {:account/name "Cars" :account/parent "Savings"})
          _ (add-account conn {:account/name "His" :account/parent "Savings/Cars"})
          _ (add-account conn {:account/name "Hers" :account/parent "Savings/Cars"})
          _ (add-account conn "Checking")
          accounts (get-account-with-parents (d/db conn) "Savings/Cars/His")]
      (is (= ["His" "Cars" "Savings"] (mapv :account/name accounts))))))

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
          _ (add-account conn "Checking")
          after-add (find-account-by-path (d/db conn) "Checking")
          _ (delete-entity conn (:db/id after-add))
          after-delete (find-account-by-path (d/db conn) "Checking")]
      (is (not (nil? after-add)))
      (is (nil? after-delete)))))

(deftest update-an-account
  (testing "When I update and account, the account should assume the new values")
  (let [conn (create-empty-db)
        _ (add-account conn "wrong-name")
        id (find-account-id-by-path (d/db conn) "wrong-name")
        _ (update-account conn id {:account/name "right-name"
                                   :account/type :account.type/liability})
        updated (find-account (d/db conn) id)]
    (is (= "right-name" (:account/name updated)))
    (is (= :account.type/liability (:account/type updated)))))

(deftest does-an-account-exist
  (testing "account-exists? returns true if the specified root account exists"
    (let [conn (create-empty-db)
          _ (add-account conn "Checking")]
      (is (account-exists? (d/db conn) "Checking"))
      (is (account-exists? (d/db conn) {:account/name "Checking"}))))
  (testing "account-exists? returns false if the specified root account does not exist")
  (testing "account-exists? returns true if the specified non-root account exists")
  (testing "account-exists? returns false if the specified nont-root account does not exist")
  )
