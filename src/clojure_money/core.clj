(ns clojure-money.core
  (:require [datomic.api :as d :refer [create-database connect transact q]]
            [clojure-money.accounts :as a])
  (:gen-class))

(def schema (load-file "resources/datomic/schema.edn"))

(def settings (load-file "config/settings.edn"))

(def uri (:datomic-uri settings))

(defn str->date-time
  "Parses a string and returns the date value"
  [s]
  (read-string (str "#inst \"" s "\"")))

(def earliest-date #datetime "1900-01-01")

(defn get-ident
  "Returns the identifier for the database entity with the specified id"
  [db id]
  (first (d/q '[:find [?ident]
                :in $ ?id
                :where [?id :db/ident ?ident]]
              db,
              id)))

(defn init-database
  []
  (d/create-database uri)

  (println "created the database")

  (let [c (d/connect uri)]
    (d/transact c schema)
    (println "created the schema")))

(defn -main
  "main entry point for the application"
  [& args]
  (when (= "init-database" (first args))
    (init-database))
  (when (= "add-account" (first args))
    (let [conn (d/connect uri)]
      (apply a/add-account (concat [conn] (rest args)))))
  (when (= "all-accounts" (first args))

    (println "all-accounts")

    (let [conn (d/connect uri)
          accounts (a/all-accounts conn)]

      (println (str "found " (count accounts) " account(s)"))

      (doseq [a (a/all-accounts conn)]
        (println (str (:account/name a) ": " (:account/type a) ": " (:account/balance a)))))))
