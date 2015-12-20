(ns clj-money.budgets-test
  (:require [clojure.test :refer :all]
            [datomic.api :as d :refer [db touch]]
            [clj-time.core :as time]
            [clj-money.util :refer :all]
            [clj-money.budgets :refer :all]
            [clj-money.accounts :refer :all]
            [clj-money.common :refer :all])
  (:use clj-money.test-common))

(defn prepare-db
  "Prepares the database for budget tests"
  []
  (let [conn (create-empty-db)]
    (add-account conn {:account/name "Salary"    :account/type :account.type/income})
    (add-account conn {:account/name "Groceries" :account/type :account.type/expense})
    (add-account conn {:account/name "Checking"  :account/type :account.type/asset})
    conn))

(def budget-attributes {:budget/name "2016"
                        :budget/start-date #inst "2016-01-01"})

(deftest validate-a-budget
  (testing "a budget without a name is invalid"
    (let [db (d/db (create-empty-db))
          to-validate (dissoc budget-attributes :budget/name)
          errors (validate-budget db to-validate)]
      (is (= ["A budget must have a name"] errors))))
  (testing "a budget without a start date is invalid"
    (let [db (d/db (create-empty-db))
          to-validate (dissoc budget-attributes :budget/start-date)
          errors (validate-budget db to-validate)]
      (is (= ["A budget must have a start date"] errors))))
  (testing "a budget start date can be a parsable string"
    (let [db (d/db (create-empty-db))
          to-validate (assoc budget-attributes :budget/start-date "2016-01-01")
          errors (validate-budget db to-validate)]
      (is (empty? errors))))
  (testing "a budget start date can be a java.util.Date"
    (let [db (d/db (create-empty-db))
          errors (validate-budget db budget-attributes)]
      (is (empty? errors))))
  (testing "a budget start date can be a org.joda.time.DateTime"
    (let [db (d/db (create-empty-db))
          to-validate (assoc budget-attributes :budget/start-date (time/date-time 2016 1 1))
          errors (validate-budget db to-validate)]
      (is (empty? errors))))
  (testing "a budget start date cannot be an unparsable string"
    (let [db (d/db (create-empty-db))
          to-validate (assoc budget-attributes :budget/start-date "notadate")
          errors (validate-budget db to-validate)]
      (is (= ["A budget must have a valid start date"] errors)))))

(deftest add-a-budget
  (testing "When I add a budget, it appears in the list of budgets"
    (let [conn (create-empty-db)
          _ (add-budget conn {:budget/name "2015"
                              :budget/start-date #inst "2015-01-01"})
          budgets (map #(into {} %) (all-budgets (d/db conn)))]
      (is (= budgets [{:budget/name "2015" :budget/start-date #inst "2015-01-01"}]))))
  (testing "When I add a budget, I can find it by name"
    (let [conn (create-empty-db)
          _ (add-budget conn {:budget/name "2016"
                              :budget/start-date #inst "2016-01-01"})
          budget (into {} (find-budget-by-name (d/db conn) "2016"))]
      (is (= budget {:budget/name "2016" :budget/start-date #inst "2016-01-01"})))))

(def budget-item-attributes
  {:budget/_items (:budget/name budget-attributes)
   :budget-item/account "Groceries"
   :budget-item/periods (map #(hash-map :budget-item-period/index %
                                        :budget-item-period/amount 300M)
                             (range 0 12))})

(deftest validate-a-budget-item
  (testing "A valid budget receives no validation errors"
    (let [conn (prepare-db)
          _ (add-budget conn budget-attributes)
          errors (validate-budget-item (d/db conn) budget-item-attributes)]
      (is (empty? errors))))
  (testing "A budget item must reference a budget"
    (let [conn (prepare-db)
          _ (add-budget conn budget-attributes)
          errors (validate-budget-item (d/db conn) (dissoc budget-item-attributes :budget/_items))]
      (is (= ["A budget item must reference a budget"] errors))))
  (testing "A budget item must reference an account"
    (let [conn (prepare-db)
          _ (add-budget conn budget-attributes)
          errors (validate-budget-item (d/db conn) (dissoc budget-item-attributes :budget-item/account))]
      (is (= ["A budget item must reference an account"] errors))))
  (testing "A budget item without any periods is invalid"
    (let [conn (prepare-db)
          _ (add-budget conn budget-attributes)
          errors (validate-budget-item (d/db conn) (dissoc budget-item-attributes :budget-item/periods))]
      (is (= ["A budget item must have 12 periods"] errors))))
  (testing "A budget item with more than 12 periods is invalid"
    (let [conn (prepare-db)
          _ (add-budget conn budget-attributes)
          errors (validate-budget-item (d/db conn) (update budget-item-attributes
                                                           :budget-item/periods
                                                           conj
                                                           {:budget-item-period/index 12
                                                            :budget-item-period/amount 300M}))]
      (is (= ["A budget item must have 12 periods, indexed 0 through 11"] errors))))
  (testing "A budget item with less than 12 periods is invalid"
    (let [conn (prepare-db)
          _ (add-budget conn budget-attributes)
          errors (validate-budget-item (d/db conn) (update budget-item-attributes
                                                           :budget-item/periods
                                                           rest))]
      (is (= ["A budget item must have 12 periods, indexed 0 through 11"] errors))))
  (testing "A budget item must have periods indexed 0 - 11"
    (let [conn (prepare-db)
          _ (add-budget conn budget-attributes)
          errors (validate-budget-item (d/db conn) (assoc budget-item-attributes
                                                          :budget-item/periods
                                                          (mapv #(hash-map :budget-item-period/index %
                                                                           :budget-item-period/amount 300M)
                                                                (range 1 12))))]
      (is (= ["A budget item must have 12 periods, indexed 0 through 11"] errors)))))

(deftest add-a-budget-item
  (testing "When I add an item to a budget, it should appear in the budget items attribute"
    (let [conn (prepare-db)
          _ (add-budget conn budget-attributes)
          _ (add-budget-item conn budget-item-attributes)
          first-item (-> conn
                         d/db
                         (find-budget-by-name (:budget/name budget-attributes))
                         :budget/items
                         first)
          account-name (->> first-item
                            :budget-item/account
                            d/touch
                            :account/name)
          period-amounts (->> first-item
                              :budget-item/periods
                              (sort-by :budget-item-period/index)
                              (map :budget-item-period/amount))]
      (is (= period-amounts [300M 300M 300M 300M 300M 300M 300M 300M 300M 300M 300M 300M]))
      (is (= account-name "Groceries"))))
  (testing "A budget cannot have two items with the same account"
    (let [conn (prepare-db)
          _ (add-budget conn budget-attributes)
          _ (add-budget-item conn budget-item-attributes)]
      (is (thrown-with-msg? RuntimeException
                            "A budget item already exists for account 'Groceries'"
                            (add-budget-item conn budget-item-attributes))))))

(deftest get-budget-end-date
  (testing "Given a budget, I can get the end date"
    (let [conn (prepare-db)
          _ (add-budget conn {:budget/name "2015"
                              :budget/start-date #inst "2015-01-01"})
          end-date (budget-end-date (find-budget-by-name (d/db conn) "2015"))]
      (is (= end-date #inst "2015-12-31")))))

(deftest get-a-budget-amount
  (testing "Given a budget, and account, and a number of periods, I can get a budget amount"
    (let [conn (prepare-db)
          _ (add-budget conn budget-attributes)
          _ (add-budget-item conn budget-item-attributes)
          budget-amount (get-budget-amount (d/db conn) (:budget/name budget-attributes) "Groceries" 3)]
      (is (= 900M budget-amount)))))

(deftest get-a-budget-from-a-date
  (testing "Given a date, I can get the budget that includes that date"
    (let [conn (prepare-db)
          _ (add-budget conn {:budget/name "2016"
                              :budget/start-date #inst "2016-01-01"})
          _ (add-budget conn {:budget/name "2015"
                              :budget/start-date #inst "2015-01-01"})
          _ (add-budget conn {:budget/name "2014"
                              :budget/start-date #inst "2014-01-01"})
          budget-name (find-budget-containing-date (d/db conn) #inst "2015-02-27")]
      (is (= "2015" (:budget/name budget-name))))))

(deftest get-a-budget-item-period
  (testing "Given a budget item, I can get the budget period"
    (let [conn (prepare-db)
          _ (add-budget conn budget-attributes)
          _ (add-budget-item conn {:budget/_items (:budget/name budget-attributes)
                                   :budget-item/account "Groceries"
                                   :budget-item/periods (map-indexed #(hash-map :budget-item-period/index %1
                                                                                :budget-item-period/amount %2)
                                                                     (take 12 (iterate (partial + 100M) 100M))) })
          budget-item-period (-> conn
                                 d/db
                                 (find-budget-item-period (:budget/name budget-attributes) "Groceries" #inst "2016-03-02"))]
      (is (= 300M (:budget-item-period/amount budget-item-period))))))

(deftest update-a-budget
  (testing "A budget name can be updated"
    (let [conn (prepare-db)
          _ (add-budget conn {:budget/name "Twenty Fifteen"
                              :budget/start-date #inst "2015-01-01"})
          {budget-id :db/id} (find-budget-by-name (d/db conn) "Twenty Fifteen")
          _ (update-budget conn {:db/id budget-id
                                 :budget/name "2015" })
          retrieved (find-budget (d/db conn) budget-id)]
      (is (= "2015" (:budget/name retrieved)))
      (is (= #inst "2015-01-01" (:budget/start-date retrieved)))))
  (testing "A budget start date can be updated"
    (let [conn (prepare-db)
          _ (add-budget conn {:budget/name "2015"
                              :budget/start-date #inst "2015-12-31"})
          {budget-id :db/id} (find-budget-by-name (d/db conn) "2015")
          _ (update-budget conn {:db/id budget-id
                                 :budget/start-date #inst "2015-01-01"})
          retrieved (into {} (find-budget-by-name (d/db conn) "2015"))]
      (is (= "2015" (:budget/name retrieved)))
      (is (= #inst "2015-01-01" (:budget/start-date retrieved))))))
