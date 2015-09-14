(ns clj-money.budgets
  (:require [datomic.api :as d :refer [transact q db]]
            [clj-money.common :as m :refer :all]
            [clj-money.accounts :refer :all]
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

(defn add-budget
  "Adds a new budget to the system"
  [conn budget-name start-date]
  (let [new-id (d/tempid :db.part/user)
        tx-data {:db/id new-id
                 :budget/name budget-name
                 :budget/start-date start-date}]
    @(d/transact conn [tx-data])))

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
  [budget account]
  (->>
    budget
    :budget/items
    (filter #(= (:db/id account) (:db/id (:budget-item/account %))))
    first))

(defn resolve-budget
  [db budget-or-name]
  (if (string? budget-or-name)
    (find-budget-by-name db budget-or-name)
    budget-or-name))

(defn add-budget-item
  "Adds a line item to a budget"
  [conn budget-or-name account-or-path amounts]
  (let [budget (resolve-budget (d/db conn) budget-or-name)
        account (resolve-account (d/db conn) account-or-path)
        periods (map-indexed
                  #(hash-map :budget-item-period/index %1 :budget-item-period/amount %2)
                  amounts)]

    (if-not budget (throw (IllegalArgumentException. (str "Unable to find a budget named \"" budget-or-name "\""))))
    (if-not account (throw (IllegalArgumentException. (str "Unable to find an account with the path \"" account-or-path "\""))))

    (let [existing (find-budget-item budget account)
          insert [{:db/id (:db/id budget)
                   :budget/items [{:budget-item/account (:db/id account)
                                   :budget-item/periods periods}]}]
          tx-data (if existing
                    (cons [:db.fn/retractEntity (:db/id existing)] insert)
                    insert)]
      @(d/transact conn tx-data))))

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
        item (find-budget-item budget account)]
    (reduce #(+ %1 (:budget-item-period/amount %2))
            (bigdec 0)
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
        budget-item (find-budget-item budget account)]
    (->> budget-item
         :budget-item/periods
         (map #(append-budget-item-period-dates % budget-start-date))
         (filter (fn [{:keys [start-date end-date]}] (between? (c/from-date date) start-date end-date)))
         first)))
