(ns clj-money.web.transactions
  (:require [clojure.tools.logging :as log]
            [clj-money.transactions :as transactions]
            [clj-money.accounts :as accounts]
            [clj-money.common :as common]
            [clj-money.web.layouts :refer :all]
            [datomic.api :as d]
            [ring.util.anti-forgery :refer :all]
            [ring.util.response :refer [redirect]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]))

(defn index-transactions
  []
  (main-layout
    "Transactions"
    [:div.page-header
     [:h1 "Transactions"]]
    [:table.table.table-striped
     [:tr
      [:th "Date"]
      [:th "Description"]
      [:th "Amount"]]]))
