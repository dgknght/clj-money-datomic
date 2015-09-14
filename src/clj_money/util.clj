(ns clj-money.util
  (:require [clojure.pprint :refer :all]
            [clj-time.format :as f]
            [clojure.tools.logging :as log]))

(defn parse-date
  [value]
  (f/parse (:date f/formatters) value))

(defn parse-date-time
  [value]
  (f/parse (:date-time f/formatters) value))

(defn println-and-return
  "Prints the specified information and then returns it"
  ([caption to-print] (println-and-return caption identity to-print))
  ([caption transform-fn to-print]
   (println (str "*** " caption " ***"))
   (if (coll? to-print)
     (doseq [p to-print]
       (println (str "  " (transform-fn p))))
     (println (str "  " (transform-fn to-print))))
   (println (str "*** end - " caption " ***"))))

(defn pp-str
  [data]
  (let [w (java.io.StringWriter.)]
    (pprint data w)
    (.toString w)))
