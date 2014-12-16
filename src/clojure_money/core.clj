(ns clojure-money.core
  (:require [datomic.api :as d :refer :all])
  (:gen-class))

(def schema (load-file "resources/datomic/schema.edn"))

(def conn nil)

(defn str->date-time
  "Parses a string and returns the date value"
  [s]
  (read-string (str "#inst \"" s "\"")))
