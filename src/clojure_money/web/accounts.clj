(ns clojure-money.web.accounts
  (:require [clojure-money.accounts :as accounts]
            [clojure-money.web.layouts :refer :all]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]))

(defn index-accounts []
  (main-layout
    "Accounts"
    [:div {:class "page-header"}
     [:h1 "Accounts"]]
    [:p  "Coming soon."]))
