(ns clojure-money.web.layouts
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]))

(defn main-layout
  [title & content]
  (html
    ["<!DOCTYPE html>" 
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
       [:link  {:rel "stylesheet" :href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"}]
       [:link  {:rel "stylesheet" :href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap-theme.min.css"}]
       [:script  {:src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"}]

       "<!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->"
       "<!-- [if lt IE 9]>\n      <script src=\"https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js\"></script>\n      <script src=\"https://oss.maxcdn.com/respond/1.4.2/respond.min.js\"></script>\n    <!  [endif]-->"]  

      [:body  
       [:nav  {:class "navbar navbar-inverse navbar-fixed-top"}  
        [:div  {:class "container"}  
         [:div  {:class "navbar-header"}  
          [:button  {:type "button", :class "navbar-toggle collapsed", :data-toggle "collapse", :data-target "#navbar", :aria-expanded "false", :aria-controls "navbar"}  
           [:span  {:class "sr-only"} "Toggle navigation"]  
           [:span  {:class "icon-bar"}]  
           [:span  {:class "icon-bar"}]  
           [:span  {:class "icon-bar"}]]  
          [:a  {:class "navbar-brand", :href "#"} "clj-money"]]  
         [:div  {:id "navbar", :class "collapse navbar-collapse"}  
          [:ul  {:class "nav navbar-nav"}  
           [:li  {:class "active"} 
            [:a  {:href "#"} "Home"]]  
           [:li 
            [:a  {:href "#about"} "About"]]  
           [:li 
            [:a  {:href "#contact"} "Contact"]]]] "<!--/.nav-collapse -->"]]  
       [:div  {:class "container"} content]  
       [:script  {:src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"}]  
       ]]]))
