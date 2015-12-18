(ns clj-money.web.layouts
  (:require [ring.util.anti-forgery :refer :all]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]))

(defn navigation
  []
  [:nav  {:class "navbar navbar-inverse navbar-fixed-top"}
   [:div  {:class "container"}
    [:div  {:class "navbar-header"}
     [:button  {:type "button", :class "navbar-toggle collapsed", :data-toggle "collapse", :data-target "#navbar", :aria-expanded "false", :aria-controls "navbar"}
      [:span  {:class "sr-only"} "Toggle navigation"]
      [:span  {:class "icon-bar"}]
      [:span  {:class "icon-bar"}]
      [:span  {:class "icon-bar"}]]
     [:a  {:class "navbar-brand", :href "/"} "clj-money"]]
    [:div  {:id "navbar", :class "collapse navbar-collapse"}
     [:ul  {:class "nav navbar-nav"}
      [:li
       [:a {:href "/accounts"} "Accounts"]]
      [:li
       [:a {:href "/transactions"} "Transactions"]]
      [:li
       [:a {:href "/budgets"} "Budgets"]]]]]])

(defn main-layout
  [title & content]
  (html5
    [:html  {:lang "en"}
     [:head
      [:meta  {:charset "utf-8"}]
      [:meta  {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
      [:meta  {:name "viewport" :content "width=device-width, initial-scale=1"}]
      "<!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->"

      [:meta  {:name "description" :content "Double-entry account system"}]
      [:meta  {:name "author" :content "Doug Knight"}]
      [:link  {:rel "icon" :href "../../favicon.ico"}]
      [:title (str "clj-money - " title)]

      "<!-- jQuery -->"
      [:script {:src "http://code.jquery.com/jquery-2.1.4.min.js"}]

      "<!-- Bootstrap core CSS -->"
      [:link {:rel "stylesheet" :href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"}]
      [:link {:rel "stylesheet" :href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap-theme.min.css"}]
      [:link {:rel "stylesheet" :href "/clj-money.css"}]
      [:script  {:src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"}]]
     [:body
      (navigation)
      [:div.container {:style "margin-top: 2em;"}  content]
      [:script  {:src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"}]
      ]]))

(defn delete-form
  [model-type id]
  [:form.form-inline {:action (str "/" model-type "/" id "/delete")
                        :method "POST"
                        :style "margin: 0; padding: 0;"}
     (anti-forgery-field)
     [:button.btn.btn-sm.btn-link {:type "submit" :title (str "Click here to delete the " model-type ".")}
      [:span.glyphicon.glyphicon-remove {:aria-hidden true}]]])
