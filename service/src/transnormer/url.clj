(ns transnormer.url
  (:require
   [reitit.core]
   [ring.util.request]
   [lambdaisland.uri :as uri]))

(defn index
  ([req]
   (index req nil))
  ([{:reitit.core/keys [router] :as req} q]
   (let [match (reitit.core/match-by-name router :index)
         path  (reitit.core/match->path match (when q {:q q}))]
     (str (uri/join (ring.util.request/request-url req) path)))))
