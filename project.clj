(defproject codes.bauer/LiSP2022 "0.0.1"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [com.hyperfiddle/rcf "20220405"]]
  :profiles {:test {:jvm-opts ["-Dhyperfiddle.rcf.generate-tests=true"]
                    ;; src contains test forms
                    :test-paths ["src"]}})
