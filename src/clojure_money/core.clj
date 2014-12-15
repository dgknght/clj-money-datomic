(ns clojure-money.core
  (:require [datomic.api :as d :refer :all])
  (:gen-class))

;TODO Move this to a separate file
(def schema (load-file "resources/datomic/schema.edn"))

(def conn nil)

(defn add-account
  "Saves an account to the database"
  ([account-name] (add-account account-name "asset"))
  ([account-name account-type]
   (let [new-id (d/tempid :db.part/user)]
     @(d/transact
        conn
        [{:db/id new-id
          :account/name account-name
          :account/type account-type}]))))

(defn all-accounts
  "Returns all of the accounts in the system"
  []
  (d/q
    '[:find [?account-name ?account-type]
      :where [_ :account/name ?account-name]
             [_ :account/type ?account-type]]
    (d/db conn)))

(defn find-account-by-path
  "Finds an account with the specified path"
  [path]
  (d/pull (d/db conn) '[*] [:account/name path]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
