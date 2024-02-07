(ns transnormer.env
  (:import
   (io.github.cdimascio.dotenv Dotenv)))

(def ^Dotenv dot-env
  (.. (Dotenv/configure) (ignoreIfMissing) (load)))

(defn get-var
  ([k]
   (get-var k nil))
  ([k dv]
   (let [k (str "TRANSNORMER_" k)]
     (or (System/getenv k) (.get dot-env k) dv))))

(def debug?
  (some? (not-empty (get-var "DEBUG"))))

(def http-port
  (parse-long (get-var "HTTP_PORT" "8080")))

(def http-context-path
  (get-var "HTTP_CONTEXT_PATH" ""))

(def model-device
  (get-var "MODEL_DEVICE" "cpu"))

(def model-dir
  (get-var "MODEL_DIR" "model"))
