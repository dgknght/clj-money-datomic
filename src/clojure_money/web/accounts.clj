(ns clojure-money.web.accounts
  (:require [clojure-money.accounts :as accounts]
            [clojure-money.web.layouts :refer :all]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]))

(defn index-accounts []
  (main-layout
    "Accounts"
    [:h2 "Accounts"]
    [:p "Coming soon."]))
