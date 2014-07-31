(ns app.core
  (:require [clojure.java.io :as io])
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:require [instaparse.core :as insta])
  (:gen-class :main true))

; ============================================
; Misc
; ============================================

(def not-nil? (complement nil?))

; ============================================
; Files
; ============================================

(defn java-file? [f]
  (and (.isFile f)
    (.endsWith (.getName f) ".java")))

(defn java-files [dirname]
  (let [d (java.io.File. dirname)]
    (filter java-file? (file-seq d))))

(load "javaparser")
(load "operations")
(load "itemsOnLifecycle")
(load "interactive")
(load "cli")

; TODO extract part of this method to cli
(defn -main
  "What I do"
  [& args]
  (let [optsMap
    (parse-opts args
      [
        ["-h" "--help" "Show help" :flag true :default false]
        ["-i" "--interactive" "launch interactive move" :flag true :default false]
        ["-d" "--dir DIRNAME" "REQUIRED: Directory containing the code to check"]
        ["-q" "--query QUERYNAME" "REQUIRED: Query to perform: mc=many constructors, mcp=many constructor parameters, st=singleton type"]
        ["-t" "--threshold VALUE" "Threshold to be used in the query" :default 0
         :parse-fn #(Integer/parseInt %)
         :validate [#(>= % 0) "Must be a number equal or greater to 0"]]
      ])
    opts (:options optsMap)
    banner (:summary optsMap)]
    (do
      (when (:interactive opts)
        (do
          (interactive {})
          (System/exit 0)))
      (when (:help opts)
        (do
          (println ("Printing help message, as asked"))
          (println banner))
        (System/exit 0))
      (if
        (and
          (:dir opts)
          (:query opts)
          (name2operation (:query opts))
          (nil? (:errors opts)))
        (run opts)
        (do
          (println "Incorrect usage")
          (when (:errors opts)
            (doseq [e (:errors opts)]
              (println " * " e)))
          (println banner)
          (System/exit 1))))))
