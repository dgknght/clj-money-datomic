(ns clojure-money.accounts
  (:require [datomic.api :as d :refer :all]
            [clojure-money.core :refer [conn]])
  (:gen-class))

(def left-side-types [:account.type/asset :account.type/expense])
(def right-side-types [:account.type/liability :account.type/equity :account.type/income])

(defn add-account
  "Saves an account to the database"
  ([account-name] (add-account account-name :account.type/asset))
  ([account-name account-type]
   (let [new-id (d/tempid :db.part/user)]
     @(d/transact
        conn
        [{:db/id new-id
          :account/name account-name
          :account/type account-type
          :account/balance (bigdec 0)}]))))

(defn all-accounts
  "Returns all of the accounts in the system"
  []
  (let [db (d/db conn)]
    (->> (d/q
           '[:find ?a
             :where [?a :account/name]]
           db)
         (map #(d/entity db (first %)))
         (map #(d/touch %)))))

(defn find-account-by-path
  "Finds an account with the specified path"
  [path]
  (let [db  (d/db conn)]
    (->> path
         (d/q
             '[:find [?a]
               :in $ ?account-name
               :where [?a :account/name ?account-name]]
             db)
         first
         (d/entity db)
         d/touch)))

(defn find-account-id-by-path
  "Finds an account with the specified path"
  [path]
  (let [db  (d/db conn)]
    (->> path
         (d/q
             '[:find [?a]
               :in $ ?account-name
               :where [?a :account/name ?account-name]]
             db)
         first)))

(defn left-side?
  "Returns true if the account is asset or expense, otherwise false"
  [account]
  (let [type (:account/type account)]
    (some #(= type %) left-side-types)))

(defn right-side?
  "Returns true if the account is liability, equity or income, otherwise false"
  [account]
  (not (left-side? account)))

(defn polarizer
  "Return -1 or 1, by which a transaction amount can be multiplied to affect an account balance based on the account type and action (credit or debit)"
  [account action]
  (if (or (and (left-side? account) (= :transaction-item.action/debit action))
          (and (right-side? account) (= :transaction-item.action/credit action)))
    1
    -1))

(defn adjust-balance
  "Adjusts the balance of an account"
  [path amount action]
  (let [account (find-account-by-path path)
        pol (polarizer account action)
        polarized-amount (* pol amount)]
    @(d/transact conn [[:db/add (:db/id account) :account/balance polarized-amount]])))

(defn debit-account
  "Debits the specified account"
  [path amount]
  (adjust-balance path amount :transaction-item.action/debit))

(defn credit-account
  "Debits the specified account"
  [path amount]
  (adjust-balance path amount :transaction-item.action/credit))

(defn get-balance
  "Gets the balance for the specified account"
  [path]
  (first (d/q
           '[:find [?balance]
             :in $ ?account-name
             :where [?a :account/name ?account-name]
                    [?a :account/balance ?balance]]
           (d/db conn)
           path)))
