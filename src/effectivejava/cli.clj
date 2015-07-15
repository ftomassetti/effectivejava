(ns effectivejava.cli
  (:use [effectivejava.model.protocols])
  (:use [effectivejava.model.javaparser])
  (:use [effectivejava.javaparser.navigation])
  (:use [effectivejava.operations])
  (:use [effectivejava.itemsOnLifecycle])
  (:use [effectivejava.utils])
  (:require [instaparse.core :as insta])
  (:import [effectivejava.operations Operation]))

; ============================================
; CLI
; ============================================

(def cliOpts [["-h" "--help" "Show help" :flag true :default false]
              ["-i" "--interactive" "launch interactive mode" :flag true :default false]
              ["-l" "--linter" "launch linter mode" :flag true :default false]
              ["-d" "--dir DIRNAME" "directory containing the code to check (default current dir)" :default "."]
              ["-q" "--query QUERYNAME" "REQUIRED: Query to perform: mc=many constructors, mcp=many constructor parameters, st=singleton type, ts=overrides toString()?, u=utils classes"]
              ["-t" "--threshold VALUE" "Threshold to be used in the query" :default 0
               :parse-fn #(Integer/parseInt %)
               :validate [#(>= % 0) "Must be a number equal or greater to 0"]]])

(def operations
  {:f finalizersOp
   :mc classesWithManyConstructorsOp
   :mcp constructorsWithManyParametersOp
   :st classesAndSingletonTypeOp
   :ts toStringOp
   :u utilsClassesOp})

(defn run [opts]
  (let [dirname (:dir opts)
        th (:threshold opts)
        operation ((keyword (:query opts)) operations)
        cus (filter not-nil? (cus dirname))]
    (println "Considering" (.size cus) "Java files")
    (printOperation operation cus th)))
