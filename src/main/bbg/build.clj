(ns bbg.build
  (:require [aero.core :as aero]
            [babashka.fs :as fs]
            [bbg.util :as util :refer [when-pred]]

            [cheshire.core :as json]
            [clojure.string :as string]))

;; Only version of form 'a.b.c(.d)' is accepted
;; as :version in manifest
(defmethod aero.core/reader 'manifest-version
  [_opts _tag version]
  (let [version-re #"v(\d+\.\d+\.\d+)"]
    (->> version
         name
         string/trim
         (re-find version-re)
         last)))

(defn- project-root []
  (->> (util/traverse-up)
       (filter #(util/file-exists?-> % "shadow-cljs.edn"))
       first))

(defn- version []
  (when-first [git-dir (keep #(util/file-exists?-> % ".git") (util/traverse-up))]
    (string/trim (util/git "--git-dir" (str git-dir) "describe" "--tags"))))

(defn write-VERSION
  "Writes `(version)` to $PROJECT_ROOT/VERSION when absent or when dev-mode.
  So whenever a VERSION exists and we're releasing, that existing VERSION is leading."
  {:shadow.build/stage :configure}
  [{:shadow.build/keys [mode] :as build-state}]
  (let [version-file   (fs/file (project-root) "VERSION")
        write-version? (or (not (fs/exists? version-file))
                           (= mode :dev))]
    (when write-version?
      (spit (fs/file (project-root) "VERSION") (version))))
  build-state)

(defn manifest
  {:shadow.build/stage :compile-prepare}
  [{:shadow.build/keys [mode] :as build-state}]
  (-> "manifest.edn"
      (aero/read-config {:profile mode})
      (json/generate-string {:pretty true})
      (->> (spit "ext/manifest.json")))
  build-state)
