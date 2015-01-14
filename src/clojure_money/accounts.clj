(ns clojure-money.accounts
  (:require [datomic.api :as d :refer [transact q db]])
  (:gen-class))

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
  [db]
  (->> (d/q
         '[:find ?a
           :where [?a :account/name]]
         db)
       (map #(d/entity db (first %)))
       (map #(d/touch %))))

(defn find-account-by-path
  "Finds an account with the specified path"
  [db path]
  (->> path
       (d/q
         '[:find [?a]
           :in $ ?account-name
           :where [?a :account/name ?account-name]]
         db)
       first
       (d/entity db)
       d/touch))

(defn find-account-id-by-path
  "Finds an account with the specified path"
  [db path]
  (->> path
       (d/q
         '[:find [?a]
           :in $ ?account-name
           :where [?a :account/name ?account-name]]
         db)
       first))

(def left-side?
  (d/function '{:lang :clojure
                :params [account]
                :code (contains?
                        #{:account.type/asset :account.type/expense}
                        (:account/type account))}))

(def right-side?
  (d/function '{:lang :clojure
                :params [account]
                :code (not (clojure-money.accounts/left-side? account))}))

(def polarizer
  (d/function '{:lang :clojure
                :params [account action]
                :code (if (or (and (clojure-money.accounts/left-side? account) (= :transaction-item.action/debit action))
                              (and (clojure-money.accounts/right-side? account) (= :transaction-item.action/credit action)))
                        1
                        -1)}))

(def adjust-balance
  (d/function '{:lang :clojure
                :params [db id amount action]
                :code (let [e (d/entity db id)
                            account (d/touch e)
                            pol (clojure-money.accounts/polarizer account action)
                            current-balance (:account/balance account)
                            polarized-amount (* pol amount)
                            new-balance (+ current-balance polarized-amount)]
                        [[:db/add id :account/balance new-balance]])}))

(defn debit-account
  "Debits the specified account"
  [conn id amount]
  @(d/transact conn [[:adjust-balance id amount :transaction-item.action/debit]]))

(defn credit-account
  "Debits the specified account"
  [conn id amount]
  @(d/transact conn [[:adjust-balance id amount :transaction-item.action/credit]]))

(defn get-balance
  "Gets the balance for the specified account"
  [db id]
  (first (d/q
           '[:find [?balance]
             :in $ ?a
             :where [?a :account/balance ?balance]]
           db
           id)))
