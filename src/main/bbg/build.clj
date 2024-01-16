(ns bbg.build
  (:require [aero.core :as aero]
            [cheshire.core :as json]))

(defn manifest
  {:shadow.build/stage :compile-prepare}
  [{:shadow.build/keys [mode] :as build-state}]
  (-> "manifest.edn"
      (aero/read-config {:profile mode})
      (json/generate-string {:pretty true})
      (->> (spit "ext/manifest.json")))
  build-state)
