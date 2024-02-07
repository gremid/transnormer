(ns user
  (:require
   [clojure.tools.namespace.repl :as repl :refer [set-refresh-dirs]]
   [transnormer.server :as server]))

(set-refresh-dirs "dev" "src" "test")

(defn start!
  []
  (constantly true))

(def stop!
  nil)

(defn go
  []
  (alter-var-root #'stop! (constantly (server/start!))))

(defn halt
  []
  (when stop!
    (stop!)
    (alter-var-root #'stop! (constantly nil))))

(defn reset
  []
  (halt)
  (repl/refresh :after 'user/go))

(defn reset-all
  []
  (halt)
  (repl/refresh-all :after 'user/go))

(comment
  (go)
  (halt)
  (reset)
  (reset-all))
