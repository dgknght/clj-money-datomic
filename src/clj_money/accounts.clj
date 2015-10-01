(ns clj-money.accounts
  (:require [datomic.api :as d :refer [transact q db]]
            [clojure.string :as str]
            [clj-money.common :as m :refer :all]
            [clojure.tools.logging :as log])
  (:gen-class))

(def path-separator "/")

(defn find-account
  [db account-id]
  (let [entity (d/entity db account-id)]
    (d/touch entity)))

(defn left-side?
  "Returns true if the account is on the left side of the A = L + E equation. I.e., if it's an asset or an expense."
  [{account-type :account/type :as account}]
  (when-not account-type
    (throw (ex-info (str "Unable to determine whether or not the account is left side of the equation." {:account account}))))
  (contains?
    #{:account.type/asset :account.type/expense}
    account-type))

(defn right-side?
  "Returns true if the account is on the right side of the A = L + E equation. I.e., if it's a liability, equity, or income account."
  [account]
  (not (left-side? account)))

(defn polarizer
  "Returns a number by which a transaction amount can be multiplied to make the correct
  adjustment on the balance of the account to which it belongs."
  [account action]
  (if (or (and (left-side? account) (= :transaction-item.action/debit action))
          (and (right-side? account) (= :transaction-item.action/credit action)))
    1
    -1))

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

(declare flatten-accounts)
(defn flatten-account
  "Accepts an account with a :children attribute and returns a list containing 
  the specified account, followed by the children"
  [account depth]
  (let [children (flatten-accounts (:children account) (inc depth))]
    (cons (assoc (dissoc account :children) :depth depth)
          children)))

(defn flatten-accounts
  "Accepts stacked accounts and returns a flat list with a new depth attribute"
  ([accounts] (flatten-accounts accounts 0))
  ([accounts depth]
   (reduce #(concat %1 (flatten-account %2 depth)) [] accounts)))

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
  "Given an account (or an ID) and a list of all available accounts, calculates the path by looking up in the account list"
  [account accounts]
  (if-let [parent (:account/parent account)]
    (str (calculate-path-with-list parent accounts) path-separator (:account/name account))
    (:account/name account)))

(defn path-segments
  "Given a path, returns the segments that make up the path"
  [path]
  (str/split path #"\/"))

(defn find-account-id-by-path
  "Finds an account with the specified path"
  [db path]
  (let [find-account (partial find-account-id-by-name-and-parent db)]
    (reduce find-account nil (path-segments path))))

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
    (= java.lang.Long (type identifier)) identifier
    :else (find-account-id-by-path db identifier)))

(defn resolve-account
  "Resolves the information into an account id. The input may be a path, account entity, or id"
  [db token]
  (cond (string? token) (find-account-by-path db token)
        (integer? token) (d/entity db token)
        :else token))

(defn get-account-with-parents
  "Returns a sequence containing the specified account, followed by its parent, on up the chain until a root account is reached"
  [db account-token]
  (if account-token
    (let [{parent :account/parent :as account} (resolve-account db account-token)]
      (cons account (get-account-with-parents db parent)))))

(defn calculate-path
  "Given an account, calculates the path by lookup parents"
  [db account]
  (calculate-path-with-list account (get-account-with-parents db account)))

(defn balance-adjustment-tx-data
  "Returns the transaction date for adjusting the balance of an account"
  [account adjustment-amount]
  [:db/add (:db/id account)
   :account/balance (+ (:account/balance account) adjustment-amount)])

(defn get-balance
  "Gets the balance for the specified account"
  [db id-or-path]
  (let [id (resolve-account-id db id-or-path)]
    (first (d/q
             '[:find [?balance]
               :in $ ?a
               :where [?a :account/balance ?balance]]
             db
             id))))

(defn account-exists?
  "Returns a boolean value indicating whether or not the specified account exists.
  The acount may be specifiedy by path or with the same attribute map that would be 
  used to create the account"
  [db path-or-attribute-map]
  (if (string? path-or-attribute-map)
    (find-account-by-path db path-or-attribute-map)
    (let [{parent :account/parent account-name :account/name} path-or-attribute-map
          parent-path (if (string? parent)
                        parent
                        (calculate-path db (resolve-account db parent)))]
      (find-account-by-path db (str parent-path path-separator account-name)))))

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
  [context {account-type :account/type parent-token :account/parent :as attributes}]
  (if parent-token
    (let [parent (resolve-account (:db context) parent-token)
          parent-type (:account/type parent)]
      (if (= account-type parent-type)
        context
        (assoc-in context [:errors :account/parent] (str "Parent type (" parent-type ") must match the account type (" account-type ")."))))
    context))

(defn validate-attributes
  "Raises an exception if the specified attributes are not valid"
  [db attributes]
  (let [result (reduce #(%2 %1 attributes)
                       {:db db :errors {}}
                       [validate-name validate-type validate-parent])]
    (when-not (empty? (:errors result))
      (log/error "Validation failed for account attributes " (prn-str attributes) " for these reasons: " (prn-str (:errors result)))
      (throw (ex-info "The account information supplied is not valid." (:errors result))))))

(def default-account-attributes
  {:account/type :account.type/asset
   :account/balance (bigdec 0)
   :account/children-balance (bigdec 0)})

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

(defn update-account
  "Updates an existing account"
  [conn account-id {account-name :account/name
                    account-type :account/type
                    parent-id :account/parent}]
  (let [tx-data (cond-> {:db/id account-id}
                  account-name (assoc :account/name account-name)
                  account-type (assoc :account/type account-type)
                  parent-id (assoc :account/parent parent-id))]
    @(d/transact
       conn
       [tx-data])))
