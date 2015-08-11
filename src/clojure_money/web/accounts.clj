(ns clojure-money.web.accounts
  (:require [clojure-money.accounts :as accounts]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]))

(defn view-layout [title & content]
  (html
    (doctype :xhtml-strict)
    (xhtml-tag "en"
               [:head
                [:meta {:http-equiv "Content-Type"
                        :content "text/html; charset=utf-8;"}]

                ; Bootstrap
                [:link {:rel "stylesheet" :href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"}]
                [:link {:rel "stylesheet" :href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap-theme.min.css"}]
                [:script {:src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"}]

                [:title (str "clj-money - " title)]]
               [:body content])))

(defn index-accounts []
  (view-layout
    "Accounts"
    [:h2 "Accounts"]
    [:p "Coming soon."]))
