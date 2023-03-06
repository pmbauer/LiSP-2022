(ns test
  (:require
   [clojure.test]
   [hyperfiddle.rcf :as rcf]))

(def default-under-test
  '(codes.bauer.LiSP.ch01))

(defn run [{:keys [namespaces]
            :or {namespaces default-under-test}}]
  (alter-var-root (var rcf/*generate-tests*) (constantly true))
  (apply require namespaces)
  (apply clojure.test/run-tests namespaces))
