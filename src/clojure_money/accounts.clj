(ns clojure-money.accounts
  (:require [datomic.api :as d :refer [transact q db]]
            [clojure.string :as str]
            [clojure-money.common :as m :refer :all]
            )
  (:gen-class))

(defn all-accounts
  "Returns all of the accounts in the system"
  [db]
  (->> (d/q
         '[:find ?a
           :where [?a :account/name]]
         db)
       (map #(m/hydrate-entity db %))))

(defn append-children
  "Appends child accounts to their parents"
  [account all-accounts]
  (let [children (->> all-accounts
                      (filter #(= (:db/id account) (:db/id (:account/parent %))))
                      (map #(dissoc % :account/parent)))]
    (if (seq children)
      (assoc account :children children) #_(insert recursion here)
      account)))

(defn stacked-accounts
  "Returns all accounts in the system with children listed under their
  parents with the key :children"
  [db]
  (let [all (->> db
                 all-accounts
                 (sort #(compare (:account/name %1) (:account/name %2)))
                 (map entity-map->hash-map))]
    (->> all
         (filter #(not (:account/parent %)))
         (map #(append-children % all)))))

(defn root-accounts
  "Returns the accounts that do not have a parent account"
  [db]
  (filter #(not (:account/parent %)) (all-accounts db)))

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

(defn calculate-path-with-list
  "Given an account (or an ID), calculates the path by looking up parents in the database"
  [account accounts]
  (if-let [parent (:account/parent account)]
    (str (calculate-path-with-list parent accounts) "/" (:account/name account))
    (:account/name account)))

(defn find-account-id-by-path
  "Finds an account with the specified path"
  [db path]
  (let [find-account (partial find-account-id-by-name-and-parent db)]
    (reduce find-account nil (str/split path #"\/"))))

(defn find-account-by-path
  "Finds an account with the specified path"
  [db path]
  (let [id (find-account-id-by-path db path)]
    (if id
      (->> id
           (d/entity db)
           d/touch))))

(defn resolve-account-id
  "If the parameter is an account id, returns the account. If it is a string, it looks up the account by path"
  [db identifier]
  (cond
    (or (isa? clojure.lang.IPersistentMap identifier) (isa? datomic.Entity identifier)) (:db/id identifier)
    :else (find-account-id-by-path db identifier)))

(defn resolve-account
  "Resolves the information into an account id. The input may be a path, account entity, or id"
  [db token]
  (cond (string? token) (find-account-by-path db token)
        (integer? token) (d/entity db token)
        :else token))

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

(defn validate-type
  [context attributes]
  (if-let [account-type (:account/type attributes)]
    (if (contains? #{:account.type/asset
                     :account.type/liability
                     :account.type/equity
                     :account.type/income
                     :account.type/expense} account-type)
      context
      (assoc-in context [:errors :account/type] "Type must be asset, liability, equity, income, or expense."))
    (assoc-in context [:errors :account/type] "Type is required")))

(defn validate-name
  [context attributes]
  (if (:account/name attributes)
    context
    (assoc-in context [:errors :account/name] "Name is required")))

(defn validate-parent
  [context attributes]
  (if-let [parent-token (:account/parent attributes)]
    (if (= (:account/type attributes)
           (:account/type (resolve-account (:db context) parent-token)))
      context
      (assoc-in context [:errors :account/parent] "Parent must have the same account type"))
    context))

(defn validate-attributes
  "Raises an exception if the specified attributes are not valid"
  [db attributes]
  (let [result (reduce #(%2 %1 attributes)
                       {:db db :errors {}}
                       [validate-name validate-type validate-parent])]
    (when-not (empty? (:errors result))
      (throw (ex-info "The account information supplied is not valid." (:errors result))))))

(def default-account-attributes
  {:account/type :account.type/asset
   :account/balance (bigdec 0)})

(defn add-account
  "Saves an account to the database"
  ([conn name-or-attributes]
   (let [attributes (if (string? name-or-attributes)
                       (assoc default-account-attributes :account/name name-or-attributes)
                       (merge default-account-attributes name-or-attributes))
         _ (validate-attributes (d/db conn) attributes)
         tx-data (cond-> attributes
                     true (assoc :db/id (d/tempid :db.part/user))
                     (contains? attributes :account/parent) (update-in [:account/parent] #(resolve-account-id (d/db conn) %)))]
     @(d/transact
        conn
        [tx-data]))))

(defn add-accounts
  "Saves multiple accounts to the database"
  [conn accounts]
  (mapv #(add-account conn %) accounts))

(defn delete-account
  "Removes an account from the system"
  [conn account-id]
  (let [id (d/q '[:find ?a .
                  :in $ ?name
                  :where [?a :account/name ?name]]
                (d/db conn) "Checking")
        entity (d/entity (d/db conn) id)]
    @(d/transact conn
                 [[:db.fn/retractEntity (:db/id entity)]])))
