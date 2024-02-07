(ns transnormer.server
  (:gen-class)
  (:require
   [clojure.core.memoize :as memoize]
   [clojure.string :as str]
   [transnormer.env :as env]
   [transnormer.html :as html]
   [transnormer.url :as url]
   [lambdaisland.hiccup :as h]
   [muuntaja.core :as m]
   [reitit.ring]
   [reitit.ring.coercion]
   [reitit.ring.middleware.dev]
   [reitit.ring.middleware.exception]
   [reitit.ring.middleware.muuntaja]
   [ring.middleware.defaults]
   [ring.util.response :as resp]
   [taoensso.timbre :as log]
   [transnormer.process :as process])
  (:import
   (org.eclipse.jetty.server Server)))

(require 'transnormer.util)
(require 'ring.adapter.jetty)

(defn html-response
  [doc]
  (-> (resp/response (h/render doc)) (resp/content-type "text/html")))

(defn req->query
  [req]
  (some->
   (or (some-> req :form-params (get "q"))
       (some-> req :query-params (get "q")))
   (str/trim)
   (not-empty)))

(def align-normalizations
  (memoize/fifo process/align-normalizations {} :fifo/threshold 128))

(defn handle-request
  [{:keys [request-method] :as req}]
  (let [q (req->query req)]
    (if (= :post request-method)
      (resp/redirect (url/index req q) :see-other)
      (let [alignment (when q (align-normalizations q))]
        (when alignment
          (log/debugf "? %d char(s) --> %d alignment(s)"
                      (count q) (count alignment)))
        (html-response (html/index req q alignment))))))

(def handler-defaults
  (-> ring.middleware.defaults/site-defaults
      (assoc-in [:proxy] true)
      (assoc-in [:security :anti-forgery] false)))

(def defaults-middleware
  {:name ::defaults
   :wrap #(ring.middleware.defaults/wrap-defaults % handler-defaults)})

(defn proxy-headers->request
  [{:keys [headers] :as request}]
  (let [scheme      (some->
                     (or (headers "x-forwarded-proto") (headers "x-scheme"))
                     (str/lower-case) (keyword) #{:http :https})
        remote-addr (some->>
                     (headers "x-forwarded-for") (re-find #"^[^,]*")
                     (str/trim) (not-empty))]
    (cond-> request
      scheme      (assoc :scheme scheme)
      remote-addr (assoc :remote-addr remote-addr))))

(def proxy-headers-middleware
  {:name ::proxy-headers
   :wrap (fn [handler]
           (fn
             ([request]
              (handler (proxy-headers->request request)))
             ([request respond raise]
              (handler (proxy-headers->request request) respond raise))))})

(defn log-exceptions
  [handler ^Throwable e request]
  (when-not (some-> e ex-data :type #{:reitit.ring/response})
    (log/warn e (.getMessage e)))
  (handler e request))

(def exception-middleware
  (-> reitit.ring.middleware.exception/default-handlers
      (assoc :reitit.ring.middleware.exception/wrap log-exceptions)
      (reitit.ring.middleware.exception/create-exception-middleware)))

(def handler-options
  {:muuntaja   m/instance
   :middleware [proxy-headers-middleware
                defaults-middleware
                reitit.ring.middleware.muuntaja/format-middleware
                exception-middleware
                reitit.ring.coercion/coerce-exceptions-middleware
                reitit.ring.coercion/coerce-request-middleware
                reitit.ring.coercion/coerce-response-middleware]})

(def handlers
  [["/" {:name    :index
         :handler handle-request}]])

(def dev-router-options
  (cond-> {}
    env/debug? (assoc :reitit.middleware/transform
                      reitit.ring.middleware.dev/print-request-diffs)))

(def router
  (reitit.ring/router
   [env/http-context-path handler-options handlers]
   dev-router-options))

(def ring-handler
  (reitit.ring/ring-handler
   router
   (reitit.ring/routes
    (reitit.ring/redirect-trailing-slash-handler)
    (reitit.ring/create-resource-handler
     {:path (str env/http-context-path "/assets")})
    (reitit.ring/create-default-handler))))

(defn stop!
  [^Server server]
  (.stop server)
  (.join server))

(defn start!
  []
  (->> {:port env/http-port :join? false}
       (ring.adapter.jetty/run-jetty ring-handler)
       (partial stop!)))

(defn -main
  [& _]
  (let [stop! (start!)]
    (.. (Runtime/getRuntime) (addShutdownHook (Thread. stop!)))
    @(promise)))
