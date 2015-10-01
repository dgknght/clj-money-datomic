(ns clj-money.admin
  (:require [clojure.edn :as edn]
            [clojure.tools.logging :as log]
            [datomic.api :as d]
            [clj-money.common :as common]
            [clj-money.accounts :as accounts]
            [clj-money.transactions :as transactions]
            [clj-money.util :as util]))

(def schema (load-file "resources/datomic/schema.edn"))

(defn create-empty-db
  "Creates an empty database with schema at the specified URI"
  [uri]
  (d/delete-database uri)
  (d/create-database uri)
  (let [conn (d/connect uri)]
    @(d/transact conn schema)
    conn))

; TODO Remove the redundancies with this function and the one above
(defn init-database
  []
  (try
    (d/create-database common/uri)
    (log/info "created the database at " common/uri)
    (catch Exception e
      (log/error e "Unable to create the database at " common/uri)))
  (try
    (let [conn (d/connect common/uri)]
      @(d/transact conn schema))
    (catch Exception e
      (log/error e "Unable to create the schema in the database at " common/uri))))

(defn recreate-database
  "Drops the database, creates it again, and loads the schema"
  []
  (d/delete-database common/uri)
  (log/info "deleted the database at " common/uri "...")
  (init-database))

(defn load-sample-data
  "Loads sample data defined in ./resources/test-data.edn"
  []
  (let [test-data (-> "./resources/test-data.edn"
                      slurp
                      edn/read-string)
        conn (d/connect common/uri)
        accounts-to-add (->> test-data
                             :accounts
                             (remove #(accounts/account-exists? (d/db conn) %)))]
    (doseq [account accounts-to-add]
      (accounts/add-account conn account)
      (log/info "created account " (:account/name account)))
    (doseq [transaction (:transactions test-data)]
      (try
        (transactions/add-transaction conn transaction)
        (log/info "created transaction" (:transaction/description transaction) " on " (:transaction/date transaction))
        (catch Exception e
          (log/error e "Unable to add the transactions " (prn-str transaction) " due to an error."))))))
