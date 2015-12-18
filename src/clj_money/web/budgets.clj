(ns clj-money.web.budgets
  (:require [clojure.set :refer [rename-keys]]
            [clojure.tools.logging :as log]
            [datomic.api :as d]
            [ring.util.anti-forgery :refer :all]
            [ring.util.response :refer [redirect]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [clj-money.common :as common]
            [clj-money.budgets :as budgets]
            [clj-money.web.layouts :refer :all]))

(defn budget-row
  [{id :db/id budget-name :name}]
  [:tr
   [:td
    [:a {:href (str "/budgets/" id)} budget-name]]])

(defn index-budgets
  []
  (main-layout
    "Budgets"
    [:div.page-header
     [:h1 "Budgets"]]
    [:div.row
     [:div.col-md-6
      [:table.table.table-striped.table-hover
       [:tr
        [:th "Name"]
        [:th "&nbsp;"]]
       (let [conn (d/connect common/uri)
             list (budgets/all-budgets (d/db conn))]
         (map budget-row list))]
      [:a.btn.btn-primary {:href "budgets/new"} "New"]]]))

(defn period-options
  []
  (html
    [:option "month"]))

(defn form-fields
  []
  (html
    (anti-forgery-field)
    [:div.form-group
     [:label.form-label {:for "name"} "Name"]
     [:input.form-control {:type "text" :name "name" :id "name" :autofocus 1}]]
    [:div.form-group
     [:label.form-label {:for "start-date"} "Start date"]
     [:input.form-control.date-field {:name "start-date" :id "start-date"}]]
    [:div.form-group
     [:label.form-label {:for "period"} "Period"]
     [:select.form-control {:id "period" :name "period"}
      (period-options)]]
    [:div.form-group
     [:label.form-label {:for "period-count"} "Period count"]
     [:input.form-control {:type "number" :name "period-count" :id "period-count" :value 12}]]))

(defn new-budget
  []
  (main-layout
    "New budget"
    [:div.page-header
     [:h1 "New budget"]]
    [:div.row
     [:div.col-md-3
      [:form {:role "form" :action "/budgets" :method "POST"}
       (form-fields)
       [:div.btn-group
        [:button.btn.btn-primary {:type "submit"} "Save"]
        [:a.btn.btn-default {:href "/budgets"} "Cancel"]]]]]))

(defn prepare-params
  [html-params]
  (-> html-params
      (rename-keys html-params {:name :budget/name
                                :start-date :budget/start-date})
      (select-keys [:budget/name :budget/start-date])))

(defn create-budget
  [params]
  (try
    (let [conn (d/connect common/uri)
          budget (prepare-params params)]
      (budgets/add-budget conn budget))
    (redirect "/budgets")
    (catch Exception e
      (log/error e "Unable to create the new budget.")
      (html [:pre (prn-str params)]
            [:pre (prn-str e)]))))
