(ns clojure-money.core
  (:require [datomic.api :as d :refer :all])
  (:gen-class))

;TODO Move this to a separate file
(def schema (load-file "resources/datomic/schema.edn"))

(def conn nil)

(defn add-account
  "Saves an account to the database"
  [account-name]
  (let [new-id (d/tempid :db.part/user)]
    @(d/transact
      conn
      [{:db/id new-id
        :account/name account-name}])))

(defn all-accounts
  "Returns all of the accounts in the system"
  []
  (d/q
    '[:find ?a
      :where [_ :account/name ?a]]
    (d/db conn)))

(defn find-account-by-path
  "Finds an account with the specified path"
  [path]
  (d/pull (d/db conn) '[*] [:account/name path]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
