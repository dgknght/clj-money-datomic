(ns clojure-money.web.pages
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [clojure-money.web.layouts :refer :all]))

(defn home
  "Returns the home page"
  []
  (main-layout
    "clj-money"
    [:div.jumbotron {:style "margin-top: 4em;"}
     [:h1 "clj-money"]
     [:p "This is a simple double-entry accounting application written in clojure."]]))

