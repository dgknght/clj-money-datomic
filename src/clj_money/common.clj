(ns clj-money.common
  (:require [clojure.tools.logging :as log]
            [datomic.api :as d :refer [q]])
  (:gen-class))

(def earliest-date #inst "1900-01-01")

(defprotocol Storage
  "Defines function required to save and retrieve accounting information"
  (save-account [account] "Saves an account to the data store")
  #_(get-account [id-or-path] "Retrieves an account from the data store")
  (get-accounts [criteria] "Retrieves a list of accounts from the data store")
  #_(save-transaction [transaction] "Saves a transaction to the data store")
  #_(get-transaction [id start-date end-date] "Retrieves a transaction from the data store")
  #_(get-transactions [criteria] "Retrieves a list of transactions from the data store"))

; TODO shouldn't need this once we abstract out datomic
(defn get-ident
  "Returns the identifier for the database entity with the specified id"
  [db id]
  (first (d/q '[:find [?ident]
                :in $ ?id
                :where [?id :db/ident ?ident]]
              db,
              id)))

(def settings (load-file "config/settings.edn"))

; TODO move this to the datomic implementation of the storage protocol
(def uri (:datomic-uri settings))

; TODO move this to the datomic implementation of the storage protocol
(defn delete-entity
  "Removes an account from the system"
  [conn id]
  (let [entity (d/entity (d/db conn) id)]
    @(d/transact conn
                 [[:db.fn/retractEntity (:db/id entity)]])))

; TODO move this to the datomic implementation of the storage protocol
(defn entity-map->hash-map
  "Accepts an EntityMap and returns a run-of-the-mill hash map"
  [entity]
  (assoc (into {} entity)
         :db/id
         (:db/id entity)))

; TODO move this to the datomic implementation of the storage protocol
(defn hydrate-entity
  "Given an ID, returns an entity map with all the entity details"
  [db id]
  (->> id
       first
       (d/entity db)
       (d/touch)))

; TODO move this to utils
(defn meta-filter
  "Filters a sequence based on the specified meta-data"
  [meta-key meta-value items]
  (filter #(= meta-value (-> % meta meta-key)) items))
