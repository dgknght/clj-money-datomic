(ns clojure-money.core
  (:require [datomic.api :as d :refer [create-database connect transact q]])
  (:use clojure-money.common
        clojure-money.accounts)
  (:gen-class))

(defn -main
  "main entry point for the application"
  [& args]
  (when (= "init-database" (first args))
    (init-database))
  (when (= "add-account" (first args))
    (let [conn (d/connect uri)]
      (apply add-account (concat [conn] (rest args)))))
  (when (= "all-accounts" (first args))

    (println "all-accounts")

    (let [db (d/db (d/connect uri))
          accounts (all-accounts db)]

      (println (str "found " (count accounts) " account(s)"))

      (doseq [a accounts]
        (println (str (:account/name a) ": " (:account/type a) ": " (:account/balance a)))))))
