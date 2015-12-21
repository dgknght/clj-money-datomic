(ns clj-money.budgets
  (:require [datomic.api :as d :refer [transact q db]]
            [clj-money.common :as m :refer :all]
            [clj-money.accounts :refer :all]
            [clj-money.util :refer :all]
            [clj-time.core :as t]
            [clj-time.coerce :as c])
  (:gen-class))

(defn all-budgets
  "Returns a list of all budgets in the system"
  [db]
  (->> (d/q
         '[:find ?b
           :where [?b :budget/name]]
         db)
       (map #(m/hydrate-entity db %))))

(defn ^{:validation-message "A budget must have a name"} missing-name?
  [budget ctx]
  (nil? (:budget/name budget)))

(defn ^{:validation-message "A budget must have a start date"} missing-start-date?
  [budget ctx]
  (nil? (:budget/start-date budget)))

(defn ^{:validation-message "A budget must have a valid start date"} invalid-start-date?
  [{start-date :budget/start-date} ctx]
  (and start-date (cond
                    (instance? java.lang.String start-date) (not (parse-date start-date))
                    (instance? java.util.Date start-date) false
                    (instance? org.joda.time.DateTime start-date) false
                    :else true)))
(def validation-fns [#'missing-name?
                     #'missing-start-date?
                     #'invalid-start-date?])

(defn validate-budget
  [db budget]
  (validate budget validation-fns {:db db}))

(defn validate-budget!
  [db budget]
  (let [errors (validate-budget db budget)]
    (when (seq errors)
      (throw (ex-info "The budget is not valid" {:budget budget :errors errors})))))

(defn add-budget
  "Adds a new budget to the system"
  [conn budget]
  (validate-budget! (d/db conn) budget)
  (let [new-id (d/tempid :db.part/user)
        tx-data (-> budget
                    (assoc :db/id new-id)
                    (update :budget/start-date c/to-date))]
    @(d/transact conn [tx-data])))

(defn find-budget
  "Returns the budget having the specified ID"
  [db budget-id]
  (d/entity db budget-id))

(defn find-budget-by-name
  "Returns the budget having the specified name, or nil if no such budget was found"
  [db budget-name]
  (->> (d/q
         '[:find ?b
           :in $ ?budget-name
           :where [?b :budget/name ?budget-name]]
         db
         budget-name)
       first
       (m/hydrate-entity db)))

(defn find-budget-item
  "Find the item in a budget for the specified account"
  [db budget-id account-id]
  (->>
    (d/q
      '[:find ?bi
        :in $ ?budget-id ?account-id
        :where [?budget-id :budget/items ?bi]
               [?bi :budget-item/account ?account-id]]
      db
      budget-id
      account-id)
    first))

(defn resolve-budget
  [db budget-or-name]
  (if (string? budget-or-name)
    (find-budget-by-name db budget-or-name)
    budget-or-name))

(defn resolve-budget-item-budget
  [db {budget :budget/_items :as budget-item}]
  (if (string? budget)
    (assoc budget-item :budget/_items (:db/id (resolve-budget db budget)))
    budget-item))

(defn resolve-budget-item-account
  [db {account :budget-item/account :as budget-item}]
  (if (string? account)
    (assoc budget-item :budget-item/account (:db/id (resolve-account db account)))))

(defn ^{:validation-message "A budget item must reference a budget"} missing-budget?
  [budget-item ctx]
  (not (:budget/_items budget-item)))

(defn ^{:validation-message "A budget item must reference an account"} missing-account?
  [budget-item ctx]
  (not (:budget-item/account budget-item)))

(defn ^{:validation-message "A budget item must have 12 periods"} missing-periods?
  [budget-item ctx]
  (not (:budget-item/periods budget-item)))

(defn ^{:validation-message "A budget item must have 12 periods, indexed 0 through 11"} invalid-period-indexes?
  [budget-item ctx]
  (when (:budget-item/periods budget-item)
    (let [indexes (->> budget-item
                       :budget-item/periods
                       (map :budget-item-period/index)
                       (into #{}))]
      (not= #{0 1 2 3 4 5 6 7 8 9 10 11} indexes))))

(defn ^{:validation-message "A budget can only have one item for any account"} duplicate-account?
  [{budget-id :budget/_items account-id :budget-item/account} {db :db}]
  (and budget-id account-id (find-budget-item db budget-id account-id)))

(def budget-item-validation-fns
  [#'missing-budget?
   #'missing-account?
   #'missing-periods?
   #'invalid-period-indexes?
   #'duplicate-account?])

(defn validate-budget-item
  [db budget-item]
  (validate budget-item budget-item-validation-fns {:db db}))

(defn validate-budget-item!
  [db budget-item]
  (let [errors (validate-budget-item db budget-item)]
    (when (seq errors)
      (throw (ex-info "Unable to save the budget item"
                      {:budget-item budget-item
                       :errors errors})))))

(defn resolve-budget-item-refs
  [db budget-item]
  (->> budget-item
       (resolve-budget-item-budget db)
       (resolve-budget-item-account db)))

(defn add-budget-item
  "Adds a line item to a budget"
  [conn budget-item]
  (let [db (d/db conn)
        resolved (resolve-budget-item-refs db budget-item)
        _ (validate-budget-item! db resolved)
        tx-data (assoc resolved
                       :db/id
                       (d/tempid :db.part/user))]
    @(d/transact conn [tx-data])))

(defn budget-end-date
  "Returns the end date for the specified budget"
  [budget]
  (-> (:budget/start-date budget)
      c/from-date
      (t/plus (t/years 1))
      (t/minus (t/days 1))
      c/to-date))

(defn get-budget-amount
  "Returns the amount specified in the given budget for the given account over the given number of periods"
  [db budget-or-name account-or-name periods]
  (let [budget (resolve-budget db budget-or-name)
        account (resolve-account db account-or-name)
        item (find-budget-item db (:db/id budget) (:db/id account))]
    (reduce #(+ %1 (:budget-item-period/amount %2))
            0M
            (take periods (:budget-item/periods item)))))

(defn between?
  [value start end]
  (and (> 0 (compare start value))
       (> 0 (compare value end))))

(defn contains-date?
  "Returns truthy if the specified budget covers a span of time that includes the specified date"
  [budget date]
  (between? date (:budget/start-date budget) (budget-end-date budget)))

(defn find-budget-containing-date
  "Returns the first budget that spans the time that includes the specified date, or nil if no such budget can be found"
  [db date]
  (->> db
       all-budgets
       (filter #(contains-date? % date))
       first))

(defn append-budget-item-period-dates
  [budget-item-period budget-start-date]
  (let [start-date (t/plus budget-start-date (t/months (:budget-item-period/index budget-item-period)))
        end-date (-> start-date
                     (t/plus (t/months 1))
                     (t/minus (t/seconds 1)))]
    (assoc (into {} budget-item-period) :start-date start-date :end-date end-date)))

(defn find-budget-item-period
  "Returns the budget item period covering the time span that includes the specified date"
  [db budget-or-name account-or-name date]
  (let [budget (resolve-budget db budget-or-name)
        budget-start-date (c/from-date (:budget/start-date budget))
        account (resolve-account db account-or-name)
        budget-item (find-budget-item db (:db/id budget) (:db/id account))]
    (->> budget-item
         :budget-item/periods
         (map #(append-budget-item-period-dates % budget-start-date))
         (filter (fn [{:keys [start-date end-date]}] (between? (c/from-date date) start-date end-date)))
         first)))

(defn validate-budget-for-update!
  [budget]
  (let [errors (validate budget [#'invalid-start-date?])]
    (when (seq errors)
      (throw (ex-info "The budget is not valid" {:budget budget :errors errors})))))

(defn update-budget
  [conn budget]
  (validate-budget-for-update! budget)
  (let [tx-data (cond-> budget
                  (:budget/start-date budget) (update :budget/start-date to-date))]
    @(d/transact conn [tx-data])))
