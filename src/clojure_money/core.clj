(ns clojure-money.core
  (:require [datomic.api :as d :refer :all])
  (:gen-class))

(def schema (load-file "resources/datomic/schema.edn"))

(def conn nil)

(defn str->date-time
  "Parses a string and returns the date value"
  [s]
  (read-string (str "#inst \"" s "\"")))

(defn get-ident
  "Returns the identifier for the database entity with the specified id"
  [db id]
  (first (d/q '[:find [?ident]
                :in $ ?id
                :where [?id :db/ident ?ident]]
              db,
              id)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
