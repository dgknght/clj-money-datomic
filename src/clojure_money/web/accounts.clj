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
                [:title (str "clj-money - " title)]]
               [:body content])))

(defn index-accounts []
  (view-layout
    "Accounts"
    [:h2 "Accounts"]
    [:p "Coming soon."]))
