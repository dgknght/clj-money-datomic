(ns clj-money.util
  (:require [clojure.pprint :refer :all]
            [clojure.data :refer [diff]]
            [clj-time.format :as f]
            [clj-time.coerce :as c]
            [clojure.tools.logging :as log]))

(defn format-date
  [value]
  (f/unparse (:date f/formatters) (c/from-date value))) 

(defn parse-date
  [value] (->> value
       (f/parse (:date f/formatters))
       (c/to-date)))

(defn parse-date-time
  [value]
  (f/parse (:date-time f/formatters) value))

(defn parse-long
  [value]
  (if-not (empty? value)
    (java.lang.Long/parseLong value)))

(defn format-number
  [value]
  (if value
    (-> (java.text.DecimalFormat. "#,##0.00")
        (.format value))
    nil))

(defn pprint-with-caption
  [msg obj]
  (println "")
  (println msg)
  (pprint obj))

(defn pprint-diff
  [expected actual]
  (pprint-with-caption "expected" expected)
  (pprint-with-caption "actual" actual)
  (pprint-with-caption "diff" (diff expected actual)))

(defn pprint-and-return
  [msg obj]
  (pprint-with-caption msg obj)
  obj)

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
