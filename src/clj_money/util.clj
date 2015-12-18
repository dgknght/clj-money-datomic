(ns clj-money.util
  (:require [clojure.pprint :refer :all]
            [clojure.data :refer [diff]]
            [clj-time.format :as f]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [clojure.tools.logging :as log]))

(defn format-date
  [value]
  (f/unparse (:date f/formatters) (c/from-date value))) 

(def standard-date-format (f/formatter "M/d/yyyy"))

(defn parse-date-time
  [string-date-time]
  (if (re-matches #"\d{1,2}\/\d{1,2}\/\d{4}" string-date-time)
    (f/parse standard-date-format string-date-time)
    (c/from-string string-date-time)))

(defn date-part
  [date-time]
  (t/date-time (t/year date-time) (t/month date-time) (t/day date-time)))

(defn parse-date
  [string-date]
  (let [parsed (parse-date-time string-date)]
    (if parsed
      (date-part parsed)
      nil)))

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

(defn pprint-and-return<
  [obj msg]
  (pprint-with-caption msg obj)
  obj)

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
