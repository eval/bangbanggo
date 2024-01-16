(ns bbg.util
  (:require-macros [bbg.util :refer [log]])
  (:require [cljs.core.async :as async :refer [go]]
            [cljs.core.async.interop :refer [<p!]]
            goog.Uri
            goog.Uri.QueryData))

(defn when-pred
  ^{:author "Sergey Trofimov"
    :source "https://ask.clojure.org/index.php/8945/something-like-when-pred-in-the-core"}
  [pred v]
  (when (pred v) v))

;; source metosin/reitit
(defn- query-param [^goog.uri.QueryData q k]
  (let [vs (.getValues q k)]
    (if (< (alength vs) 2)
      (aget vs 0)
      (vec vs))))

;; source metosin/reitit
(defn query-params
  "Given goog.Uri, read query parameters into a Clojure map."
  [^goog.Uri uri]
  (let [^goog.Uri.QueryData q (.getQueryData uri)]
    (->> q
         (.getKeys)
         (map (juxt keyword #(query-param q %)))
         (into {}))))

(defn current-tab []
  (.then (js/chrome.tabs.query #js {:active true :currentWindow true}) first))

(defn navigate-to [url]
  (log ::navigate-to :url url)
  (go
    (let [tab (<p! (current-tab))]
      (js/chrome.tabs.update (.-id tab) #js {:url url}))))
