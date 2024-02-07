(ns transnormer.util
  (:require [taoensso.timbre :as log]))

(log/merge-config!
 {:min-level [#_["libpython-clj2.*" :warn]
              #_["tech.v3.*" :warn]
              ["org.eclipse.jetty.*" :info]
              ["*" :debug]]})

(Thread/setDefaultUncaughtExceptionHandler
 (reify Thread$UncaughtExceptionHandler
   (uncaughtException [_ thread ex]
     (log/errorf ex "Uncaught exception on [%s]." (.getName thread)))))

