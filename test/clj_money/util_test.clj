(ns clj-money.util-test
  (:require [clojure.test :refer :all]
            [clj-time.core :as time]
            [clj-time.coerce :as coerce]
            [clj-money.util :refer :all]))

(deftest test-parse-date-time
  (testing "a stadard US format date is parsed successfully"
    (is (= (time/date-time 2015 2 27) (parse-date-time "2/27/2015"))))
  (testing "'date' format is parsed correctly"
    (is (= (time/date-time 2004 3 2) (parse-date-time "2004-03-02")))))

(deftest test-date-part
  (testing "date-part returns the date part with 0 for hour, minute, second, etc."
    (is (= (coerce/from-date #inst "2015-02-27") (date-part (time/date-time 2015 2 27 12 34 56))))))

(deftest test-parse-date
  (testing "returns a date specified in string format"
    (= (time/date-time 2015 2 27) (parse-date "2/27/2015")))
  (testing "returns the date porition of a date time specified in string format"
    (= (time/date-time 2004 3 2) (parse-date "2004-03-02T16:10:30"))))
