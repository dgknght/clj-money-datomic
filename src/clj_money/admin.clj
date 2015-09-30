(ns clj-money.admin
  (:require [clojure.edn :as edn]
            [clojure.tools.logging :as log]
            [datomic.api :as d]
            [clj-money.common :as common]
            [clj-money.accounts :as accounts]
            [clj-money.util :as util]))

(def schema (load-file "resources/datomic/schema.edn"))

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
        conn (d/connect common/uri)]
    (doseq [account (:accounts test-data)]
      (accounts/add-account conn account)
      (log/info "created account " (:account/name account)))))
