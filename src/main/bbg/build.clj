(ns bbg.build
  (:require [aero.core :as aero]
            [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.string :as string]))

;; Only version of form 'a.b.c(.d)' is accepted
;; as :version in manifest
(defmethod aero.core/reader 'manifest-version
  [_opts _tag version]
  (let [version-re #"^v([1-9][0-9]*\.[1-9][0-9]*\.[1-9][0-9]*)"]
    (->> version
         name
         string/trim
         (re-find version-re)
         last)))

(defn- traverse-up
  ([] (traverse-up (fs/cwd)))
  ([p]
   (let [p (fs/normalize (fs/absolutize (fs/path p)))]
     (take-while some? (iterate fs/parent (if (fs/directory? p) p (fs/parent p)))))))

(defn- when-pred
  ^{:author "Sergey Trofimov"
    :source "https://ask.clojure.org/index.php/8945/something-like-when-pred-in-the-core"}
  [pred v]
  (when (pred v) v))

(defn- file-exists?-> [& fs]
  (when-pred fs/exists? (apply fs/file fs)))

(defn- git [& args]
  (some-> (apply p/process "git" args) :out slurp string/trimr))

(defn- project-root []
  (->> (traverse-up)
       (filter #(file-exists?-> % "shadow-cljs.edn"))
       first))

(defn- version []
  (when-first [git-dir (keep #(file-exists?-> % ".git") (traverse-up))]
    (string/trim (git "--git-dir" (str git-dir) "describe" "--tags"))))

(defn write-VERSION
  {:shadow.build/stage :configure}
  [build-state]
  (spit (fs/file (project-root) "VERSION") (version))
  build-state)

(defn manifest
  {:shadow.build/stage :compile-prepare}
  [{:shadow.build/keys [mode] :as build-state}]
  (-> "manifest.edn"
      (aero/read-config {:profile mode})
      (json/generate-string {:pretty true})
      (->> (spit "ext/manifest.json")))
  build-state)
