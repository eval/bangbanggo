(ns bbg.background
  (:require [bbg.topple :as topple]
            [bbg.util :as util :refer [log warn]]
            [cljs.core.async :as async :refer [go]]
            [cljs.core.async.interop :refer [<p!]]
            [clojure.string :as string]
            [goog.string :as gstr]
            goog.Uri))

(defonce bangs (atom {}))

(defn on-suspend []
  (warn ::on-suspend))

(defn load-bangs []
  (log ::load-bangs :loading)
  (go (let [^js res (<p! (js/fetch (js/chrome.runtime.getURL "bangs.json")))
            data    (js->clj (js/JSON.parse (<p! (.text res))))]
        (reset! bangs data)
        (log ::load-bangs :done))))

(defn- search-url->bang&search
  "Expects `url` with a query-param `q` containing e.g. \"!!gh+some+project\".
  Yields e.g. `{:bang \"gh\" :search \"some project\"}`."
  [url]
  (when-let [q (some-> url goog.Uri/parse util/query-params :q)]
    (zipmap [:bang :search] (rest (re-find #"!!([^ ]+) (.+)$" q)))))

(defn find-bang [bang]
  (get @bangs (some-> bang string/lower-case)))

(defn on-before-navigate [details]
  (log ::on-before-nativate)
  (when-some [{:keys [bang search]} (some-> details
                                            (js->clj :keywordize-keys true)
                                            :url
                                            search-url->bang&search)]
    (log ::on-before-nativate {:bang bang :search search})
    (when-let [{:strs [tpl] :as _bang} (find-bang bang)]
      (util/navigate-to (topple/render tpl {:s search})))))

(defn init []
  (log ::init)
  (load-bangs)
  (js/chrome.runtime.onSuspend.addListener #(#'on-suspend %1))
  (js/chrome.webNavigation.onBeforeNavigate.addListener #(#'on-before-navigate %1)
                                                        (clj->js {:url [{:queryContains "q=!!"}]}))
  (reset! bangs {:a 1}))

(comment
  (warn "ohnoes")
  (string/lower-case "Foo")

  (.toLowerCase "Foo")
  (async/<! (async/promise-chan))

  (+ 1 2)
  (util/query-params (goog.Uri/parse "https://google.com/path?q=and+moar&b=2"))

  (gstr/urlDecode "foo+and")
  (prn {:a/b 1})
  @bangs
  (init)
  (log 1 2)
  (log [(range 100) #{1 2 3 :a/v}])
  #_:end)
