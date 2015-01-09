(ns clojure-money.accounts
  (:require [datomic.api :as d :refer :all])
  (:gen-class))

(def left-side-types #{:account.type/asset :account.type/expense})
(def right-side-types #{:account.type/liability :account.type/equity :account.type/income})

(defn add-account
  "Saves an account to the database"
  ([conn account-name] (add-account conn account-name :account.type/asset))
  ([conn account-name account-type]
   (let [new-id (d/tempid :db.part/user)]
     @(d/transact
        conn
        [{:db/id new-id
          :account/name account-name
          :account/type account-type
          :account/balance (bigdec 0)}]))))

(defn all-accounts
  "Returns all of the accounts in the system"
  [conn]
  (let [db (d/db conn)]
    (->> (d/q
           '[:find ?a
             :where [?a :account/name]]
           db)
         (map #(d/entity db (first %)))
         (map #(d/touch %)))))

(defn find-account-by-path
  "Finds an account with the specified path"
  [conn path]
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
  [conn path]
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
  (contains? left-side-types (:account/type account)))

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
  [conn path amount action]
  (let [account (find-account-by-path conn path)
        pol (polarizer account action)
        polarized-amount (* pol amount)]
    @(d/transact conn [[:db/add (:db/id account) :account/balance polarized-amount]])))

(defn debit-account
  "Debits the specified account"
  [conn path amount]
  (adjust-balance conn path amount :transaction-item.action/debit))

(defn credit-account
  "Debits the specified account"
  [conn path amount]
  (adjust-balance conn path amount :transaction-item.action/credit))

(defn get-balance
  "Gets the balance for the specified account"
  [conn path]
  (first (d/q
           '[:find [?balance]
             :in $ ?account-name
             :where [?a :account/name ?account-name]
                    [?a :account/balance ?balance]]
           (d/db conn)
           path)))
