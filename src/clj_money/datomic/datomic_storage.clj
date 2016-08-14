(ns clj-money.datomic.datomic-storage
  (:require [clojure.tools.logging :as log]
            [datomic.api :as d :refer [q]]
            [clojure.set :refer [rename-keys]])
  (:use clj-money.common))

(def default-account-attributes
  {:type :asset
   :balance (bigdec 0)
   :children-balance (bigdec 0)})

(deftype datomify
  [attributes]
  (-> attributes
    (assoc :type (->> (:type attributes)
                      name
                      (str "account.type/")
                      keyword))
    (rename-keys {:name             :account/name
                  :type             :account/type
                  :balance          :account/balance
                  :parent           :account/parent
                  :children-balance :account/children-balance})))

(deftype DatomicStorage
  [conn]
  Storage
  (save-account
    [name-or-attributes]
    (let [attributes (if (string? name-or-attributes)
                       (assoc default-account-attributes :name name-or-attributes)
                       (merge default-account-attributes name-or-attributes))
          _ (validate-attributes (d/db conn) attributes)
          tx-data (cond-> attributes
                    true (assoc :db/id (d/tempid :db.part/user))
                    #_(contains? attributes :parent) #_(update-in [:parent] #(resolve-account-id (d/db conn) %))
                    true (datomify))]
      @(d/transact
         conn
         [tx-data])))
  (get-accounts [criteria]
    ; TODO Add logic to filter based on the specified criteria
    (->> (d/q
           '[:find ?a
             :where [?a :account/name]]
           db)
         (map #(m/hydrate-entity db %)))))
