(ns clj-money.web.routes
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [hiccup.core :refer :all]
            [clj-money.web.accounts :as accounts]
            [clj-money.web.transactions :as transactions]
            [clj-money.web.budgets :as budgets]
            [clj-money.web.pages :as pages]))

(defroutes app-routes
  (GET "/" [] (pages/home))

  (GET "/accounts" [] (accounts/index-accounts))
  (POST "/accounts" [:as {params :params}] (accounts/create-account params))
  (GET "/accounts/new" [] (accounts/new-account))
  (GET "/accounts/:id" [id] (accounts/show-account id))
  (GET "/accounts/:id/edit" [id] (accounts/edit-account id))
  (POST "/accounts/:id" [id :as {params :params}] (accounts/update-account id params))
  (POST "/accounts/:id/delete" [id] (accounts/delete-account id))

  (GET "/transactions" [] (transactions/index-transactions))
  (GET "/transactions/new" [] (transactions/new-transaction))
  (POST "/transactions" [:as {:keys [params]}] (transactions/create-transaction params))
  (GET "/transactions/:id" [id] (transactions/show-transaction id))
  (GET "/transactions/:id/edit" [id] (transactions/edit-transaction id))
  (POST "/transactions/:id" [id :as {params :params}] (transactions/update-transaction id params))
  (POST "/transactions/:id/delete" [id] (transactions/delete-transaction id))

  (GET "/budgets" [] (budgets/index-budgets))
  (GET "/budgets/new" [] (budgets/new-budget))
  (POST "/budgets" [:as {:keys [params]}] (budgets/create-budget params))
  (GET "/budgets/:id" [id] (budgets/show-budget id))
  (GET "/budgets/:id/edit" [id] (budgets/edit-budget id))
  (POST "/budgets/:id" [id :as {params :params}] (budgets/update-budget id params))
  (POST "/budgets/:id/delete" [id] (budgets/delete-budget id))

  (GET "/budgets/:budget-id/budget-items" [budget-id] (budgets/index-budget-items budget-id))
  (GET "/budgets/:budget-id/budget-items/new" [budget-id] (budgets/new-budget-item budget-id))
  (POST "/budgets/:budget-id/budget-items" [budget-id :as {:keys [params]}] (budgets/create-budget-item params))
  (GET "/budget-items/:id" [id] (budgets/show-budget-item id))
  (GET "/budget-items/:id/edit" [id] (budgets/edit-budget-item id))
  (POST "/budget-items/:id" [id :as {params :params}] (budgets/update-budget-item id params))
  (POST "/budget-items/:id/delete" [id] (budgets/delete-budget-item id))

  (route/not-found (html [:h1 "Resource not found"])))
