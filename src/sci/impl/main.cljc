(ns sci.impl.main
  "Only used for testing"
  {:no-doc true}
  (:require [sci.core :refer [eval-string]]
            #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as edn]))
  #?(:clj (:gen-class)))

;; for testing only
(defn -main [& [form bindings]]
  (prn (eval-string form {:bindings (edn/read-string bindings)})))