(ns clj-money.web.budgets
  (:require [clojure.set :refer [rename-keys]]
            [clojure.tools.logging :as log]
            [datomic.api :as d]
            [ring.util.anti-forgery :refer :all]
            [ring.util.response :refer [redirect]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [clj-money.common :as common]
            [clj-money.util :as util]
            [clj-money.budgets :as budgets]
            [clj-money.web.layouts :refer :all])
  (:import java.lang.Long))

(defn budget-row
  [{id :db/id budget-name :budget/name}]
  [:tr
   [:td
    [:a {:href (str "/budgets/" id)} budget-name]]
   [:td
    [:div.pull-left
     [:a.btn.btn-link.btn-sm {:href (str "/budgets/" id "/edit") :title "Click here to edit the budget."}
      [:span.glyphicon.glyphicon-pencil {:aria-hidden true}]]]
    (delete-form "budget" id)]])

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
        [:th.col-md-10 "Name"]
        [:th.col-md-2 "&nbsp;"]]
       (let [conn (d/connect common/uri)
             list (budgets/all-budgets (d/db conn))]
         (map budget-row list))]
      [:a.btn.btn-primary {:href "budgets/new"} "New"]]]))

(defn period-options
  []
  (html
    [:option "month"]))

(defn form-fields
  ([] (form-fields nil))
  ([budget]
   (html
     (anti-forgery-field)
     [:div.form-group
      [:label.form-label {:for "name"} "Name"]
      [:input.form-control {:type "text" :name "name" :id "name" :autofocus 1 :value (:budget/name budget)}]]
     [:div.form-group
      [:label.form-label {:for "start-date"} "Start date"]
      [:input.form-control.date-field {:name "start-date" :id "start-date" :value (util/format-date (:budget/start-date budget))}]]
     #_[:div.form-group
        [:label.form-label {:for "period"} "Period"]
        [:select.form-control {:id "period" :name "period"}
         (period-options)]]
     #_[:div.form-group
        [:label.form-label {:for "period-count"} "Period count"]
        [:input.form-control {:type "number" :name "period-count" :id "period-count" :value 12}]])))

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

(defn budget-params
  [html-params]
  (-> html-params
      (rename-keys {:name :budget/name
                    :start-date :budget/start-date})
      (update :budget/start-date util/parse-date)
      (select-keys [:budget/name :budget/start-date])))

(defn create-budget
  [params]
  (try
    (let [conn (d/connect common/uri)
          budget (budget-params params)]
      (budgets/add-budget conn budget))
    (redirect "/budgets")
    (catch Exception e
      (log/error e "Unable to create the new budget.")
      (html [:pre (prn-str params)]
            [:pre (prn-str e)]))))

(defn show-budget
  [id]
  (let [db (d/db (d/connect common/uri))
        budget (budgets/find-budget db (Long. id))]
    (main-layout
      "Budget"
      [:div.page-header
       [:h1 (str "Budget: " (:budget/name budget))]]
      [:div.row
       [:div.col-md-12
        [:table.table.table-striped.table-hover
         [:tr
          [:th "Account"]
          [:th "Monthly Avg."]
          [:th "Total"]]]]]
      [:div.row
       [:div.col-md-3
        [:div.btn-group
         [:a.btn.btn-primary {:href "/budget-items/new"} "Add item"]
         [:a.btn.btn-default {:href "/budgets"} "Back"]]]])))

(defn edit-budget
  [id]
  (let [db (d/db (d/connect common/uri))
        budget (budgets/find-budget db (Long. id))]
    (main-layout
      "Edit budget"
      [:div.page-header
       [:h1 "Edit budget"]]
      [:div.row
       [:div.col-md-3
        [:form {:role "form" :action (str "/budgets/" id) :method "POST"}
         (form-fields budget)
         [:div.btn-group
          [::button.btn.btn-primary {:type "submit"} "Save"]
          [:a.btn.btn-default {:href "/budgets"} "Cancel"]]]]])))

(defn update-budget
  [budget-id params]
  (let [id (Long. budget-id)
        budget-params (assoc (budget-params params) :db/id id)
        conn (d/connect common/uri)]
    (budgets/update-budget conn budget-params)
    (redirect "/budgets")))

(defn delete-budget
  [id]
  (let [conn (d/connect common/uri)]
    (common/delete-entity conn (Long. id)))
  (redirect "/budgets"))
