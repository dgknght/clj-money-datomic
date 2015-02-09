(ns clojure-money.common
  (:require [datomic.api :as d :refer [q]])
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
  (d/create-database uri)

  (println "created the database")

  (let [c (d/connect uri)]
    (d/transact c schema)
    (println "created the schema")))
