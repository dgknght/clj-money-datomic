(ns clojure-money.accounts
  (:require [datomic.api :as d :refer :all]
            [clojure-money.core :refer [conn]])
  (:gen-class))

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
  (first (d/q
           '[:find [?a]
             :in $ ?account-name
             :where [?a :account/name ?account-name]]
           (d/db conn)
           path)))

(defn debit-account
  "Debits the specified account"
  [path amount]
  (let [id (find-account-by-path path)]
    @(d/transact conn [[:db/add id :account/balance amount]])))

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
