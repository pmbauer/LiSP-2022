(ns test
  (:require [clojure.test]))

(def default-under-test
  '(codes.bauer.LiSP.ch01))

(defn run [{:keys [namespaces]
            :or {namespaces default-under-test}}]
  (dorun (map require namespaces))
  (dorun (map clojure.test/run-tests namespaces)))
