(ns build
  (:require
   [clojure.tools.build.api :as b]
   [clojure.tools.build.tasks.copy :as copy]))

(def ignores
  (conj copy/default-ignores ".+\\.clj$"))

(defn jar
  [& _]
  (let [basis       (b/create-basis {:project "deps.edn"})
        classes-dir "classes"
        jar-file    "transnormer.jar"]
    (b/delete {:path jar-file})
    (b/delete {:path classes-dir})
    (b/copy-dir {:src-dirs   ["src"]
                 :target-dir classes-dir
                 :ignores    ignores})
    (b/compile-clj {:basis      basis
                    :src-dirs   ["src"]
                    :ns-compile ['transnormer.server]
                    :class-dir  classes-dir})
    (b/uber {:class-dir classes-dir
             :uber-file jar-file
             :main      'transnormer.server
             :basis     basis})))

