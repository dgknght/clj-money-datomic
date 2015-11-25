(ns clj-money.common
  (:require [clojure.tools.logging :as log]
            [datomic.api :as d :refer [q]])
  (:gen-class))

(def earliest-date #inst "1900-01-01")

(defn get-ident
  "Returns the identifier for the database entity with the specified id"
  [db id]
  (first (d/q '[:find [?ident]
                :in $ ?id
                :where [?id :db/ident ?ident]]
              db,
              id)))

(def settings (load-file "config/settings.edn"))

(def uri (:datomic-uri settings))

(defn delete-entity
  "Removes an account from the system"
  [conn id]
  (let [entity (d/entity (d/db conn) id)]
    @(d/transact conn
                 [[:db.fn/retractEntity (:db/id entity)]])))

(defn entity-map->hash-map
  "Accepts an EntityMap and returns a run-of-the-mill hash map"
  [entity]
  (assoc (into {} entity)
         :db/id
         (:db/id entity)))

(defn hydrate-entity
  "Given an ID, returns an entity map with all the entity details"
  [db id]
  (->> id
       first
       (d/entity db)
       (d/touch)))

(defn meta-filter
  "Filters a sequence based on the specified meta-data"
  [meta-key meta-value items]
  (filter #(= meta-value (-> % meta meta-key)) items))
