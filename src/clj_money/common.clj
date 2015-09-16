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

(def schema (load-file "resources/datomic/schema.edn"))

(def settings (load-file "config/settings.edn"))

(def uri (:datomic-uri settings))

(defn init-database
  []
  (log/info "creating the database at " uri "...")
  (d/create-database uri)
  (log/info "created database at " uri)
  (let [c (d/connect uri)]
    (d/transact c schema)
    (log/info "created the schema in database at " uri)))

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
