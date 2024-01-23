(ns bbg.util
  #?(:clj (:require [babashka.fs :as fs]
                    [babashka.process :as p]
                    [clojure.string :as string]))
  #?(:cljs (:require-macros [bbg.util :refer [log warn error]]))
  #?(:cljs (:require [cljs.core.async :as async :refer [go]]
                     [cljs.core.async.interop :refer [<p!]]
                     goog.Uri
                     goog.Uri.QueryData)))

;; CLJC
(defn when-pred
  ^{:author "Sergey Trofimov"
    :source "https://ask.clojure.org/index.php/8945/something-like-when-pred-in-the-core"}
  [pred v]
  (when (pred v) v))


;; CLJ
#?(:clj
   (defn traverse-up
     ([] (traverse-up (fs/cwd)))
     ([p]
      (let [p (fs/normalize (fs/absolutize (fs/path p)))]
        (take-while some? (iterate fs/parent (if (fs/directory? p) p (fs/parent p))))))))

#?(:clj
   (defn file-exists?-> [& fs]
     (when-pred fs/exists? (apply fs/file fs))))

#?(:clj
   (defn git [& args]
     (some-> (apply p/process "git" args) :out slurp string/trimr)))

;; macros
#?(:clj
   (defmacro error [& args]
     `(.error js/console ~@args)))

#?(:clj
   (defmacro warn [& args]
     `(.warn js/console ~@args)))


#?(:clj
   (defmacro log [& args]
     (when-not (= :release (:shadow.build/mode &env))
       `(.log js/console ~@args))))


;; cljs
;; source metosin/reitit
#?(:cljs
   (defn- query-param [^goog.uri.QueryData q k]
     (let [vs (.getValues q k)]
       (if (< (alength vs) 2)
         (aget vs 0)
         (vec vs)))))

;; source metosin/reitit
#?(:cljs
   (defn query-params
     "Given goog.Uri, read query parameters into a Clojure map."
     [^goog.Uri uri]
     (let [^goog.Uri.QueryData q (.getQueryData uri)]
       (->> q
            (.getKeys)
            (map (juxt keyword #(query-param q %)))
            (into {})))))

#?(:cljs
   (defn current-tab []
     (.then (js/chrome.tabs.query #js {:active true :currentWindow true}) first)))

#?(:cljs
   (defn navigate-to [url]
     (log ::navigate-to :url url)
     (go
       (let [tab (<p! (current-tab))]
         (js/chrome.tabs.update (.-id tab) #js {:url url})))))
