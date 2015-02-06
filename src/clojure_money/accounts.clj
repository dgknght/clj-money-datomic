(ns clojure-money.accounts
  (:require [datomic.api :as d :refer [transact q db]]
            [clojure.string :as str])
  (:gen-class))

(defn hydrate-entity
  "Given an ID, returns an entity map with all the entity details"
  [db id]
  (->> id
       first
       (d/entity db)
       (d/touch)))

(defn all-accounts
  "Returns all of the accounts in the system"
  [db]
  (->> (d/q
         '[:find ?a
           :where [?a :account/name]]
         db)
       (map #(hydrate-entity db %))))

(declare resolve-account-id)
(defn child-accounts
  "Returns all of the children of the specified account"
  [db parent]
  (let [parent-id (resolve-account-id db parent)]
    (->> (d/q
           '[:find ?a
             :in $ ?parent
             :where [?a :account/parent ?parent]]
           db
           parent-id)
        (map #(hydrate-entity db %)))))

(defn find-account-id-by-name-and-parent
  "Returns an id for an account having the specified name and parent"
  [db parent-id account-name]
  (if parent-id
    (d/q '[:find ?a .
           :in $ ?parent-id ?account-name
           :where [?a :account/name ?account-name]
           [?a :account/parent ?parent-id]]
         db
         parent-id
         account-name)
    (d/q '[:find ?a .
           :in $ ?account-name
           :where [?a :account/name ?account-name]]
         db
         account-name)))

(defn find-account-id-by-path
  "Finds an account with the specified path"
  [db path]
  (let [find-account (partial find-account-id-by-name-and-parent db)]
    (reduce find-account nil (str/split path #"\/"))))

(defn find-account-by-path
  "Finds an account with the specified path"
  [db path]
  (->> (find-account-id-by-path db path)
       (d/entity db)
       d/touch))

(defn resolve-account-id
  "If the parameter is an account id, returns the account. If it is a string, it looks up the account by path"
  [db identifier]
  (cond
    (or (isa? clojure.lang.IPersistentMap identifier) (isa? datomic.Entity identifier)) (:db/id identifier)
    :else (find-account-id-by-path db identifier)))

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

(defn add-account
  "Saves an account to the database"
  ([conn account-name] (add-account conn account-name :account.type/asset))
  ([conn account-name account-type] (add-account conn account-name account-type nil))
  ([conn account-name account-type parent]
   (let [new-id (d/tempid :db.part/user)
         tx-data {:db/id new-id
                  :account/name account-name
                  :account/type account-type
                  :account/balance (bigdec 0)}
         parent-id (if parent (resolve-account-id (d/db conn) parent))
         all-tx-data (if parent-id (merge tx-data {:account/parent parent-id}) tx-data)]
     @(d/transact
        conn
        [all-tx-data]))))
