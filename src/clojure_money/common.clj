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

(def left-side?
  (d/function '{:lang :clojure
                :params [account]
                :code (contains?
                        #{:account.type/asset :account.type/expense}
                        (:account/type account))}))

(def right-side?
  (d/function '{:lang :clojure
                :params [account]
                :code (not (clojure-money.common/left-side? account))}))

(def polarizer
  (d/function '{:lang :clojure
                :params [account action]
                :code (if (or (and (clojure-money.common/left-side? account) (= :transaction-item.action/debit action))
                              (and (clojure-money.common/right-side? account) (= :transaction-item.action/credit action)))
                        1
                        -1)}))

(def adjust-balance
  (d/function '{:lang :clojure
                :params [db id amount action]
                :code (let [e (d/entity db id)
                            account (d/touch e)
                            pol (clojure-money.common/polarizer account action)
                            current-balance (:account/balance account)
                            polarized-amount (* pol amount)
                            new-balance (+ current-balance polarized-amount)]
                        [[:db/add id :account/balance new-balance]])}))

(def schema (load-file "resources/datomic/schema.edn"))

(def settings (load-file "config/settings.edn"))

(def uri (:datomic-uri settings))

(defn init-database
  []
  (d/create-database uri)
  (let [c (d/connect uri)]
    (d/transact c schema)
    (println "created the schema")))

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
