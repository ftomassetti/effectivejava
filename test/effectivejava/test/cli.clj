(ns effectivejava.test.cli
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:use [conjure.core])
  (:use [effectivejava.cli])
  (:use [effectivejava.core])
  (:use [effectivejava.model.protocols])
  (:use [effectivejava.model.javaparser])
  (:use [effectivejava.itemsOnLifecycle])
  (:use [effectivejava.javaparser.navigation])
  (:use [effectivejava.operations])
  (:use [clojure.test]))

(def javaparserCus (cus "test-resources/sample-codebases/javaparser/"))

(deftest issue43
  "mcp query fails when no threshold is specified"
  (let [op (:mcp operations)]
    (with-redefs [println (fn [_] 1)]
      (printOperation op javaparserCus 0))))

(deftest cli-opts-help
  (let [args ["--help"]
        res (parse-opts args cliOpts)]
    (is (= [] (:arguments res)))
    (is (= false (:linter (:options res))))
    (is (= false (:interactive (:options res))))
    (is (= true (:help (:options res))))
    (is (= nil (:errors res)))))

(deftest cli-opts-interactive-short
  (let [args ["-i"]
        res (parse-opts args cliOpts)]
    (is (= [] (:arguments res)))
    (is (= false (:linter (:options res))))
    (is (= true (:interactive (:options res))))
    (is (= false (:help (:options res))))
    (is (= nil (:errors res)))))

(deftest cli-opts-interactive-long
  (let [args ["--interactive"]
        res (parse-opts args cliOpts)]
    (is (= [] (:arguments res)))
    (is (= false (:linter (:options res))))
    (is (= true (:interactive (:options res))))
    (is (= false (:help (:options res))))
    (is (= nil (:errors res)))))

(deftest cli-opts-linter-short
  (let [args ["-l"]
        res (parse-opts args cliOpts)]
    (is (= [] (:arguments res)))
    (is (= true (:linter (:options res))))
    (is (= false (:interactive (:options res))))
    (is (= false (:help (:options res))))
    (is (= nil (:errors res)))))

(deftest cli-opts-linter-long
  (let [args ["--linter"]
        res (parse-opts args cliOpts)]
    (is (= [] (:arguments res)))
    (is (= true (:linter (:options res))))
    (is (= false (:interactive (:options res))))
    (is (= false (:help (:options res))))
    (is (= nil (:errors res)))))

(deftest cli-opts-unknown
  (let [args ["--foo"]
        res (parse-opts args cliOpts)]
    (is (:errors res))))

(deftest cli-opts-dir-short
  (let [args ["-d" "FOO"]
        res (parse-opts args cliOpts)]
    (is (= [] (:arguments res)))
    (is (= false (:linter (:options res))))
    (is (= false (:interactive (:options res))))
    (is (= false (:help (:options res))))
    (is (= "FOO" (:dir (:options res))))
    (is (= nil (:errors res)))))

(deftest cli-opts-dir-long
  (let [args ["--dir" "FOO"]
        res (parse-opts args cliOpts)]
    (is (= [] (:arguments res)))
    (is (= false (:linter (:options res))))
    (is (= false (:interactive (:options res))))
    (is (= false (:help (:options res))))
    (is (= "FOO" (:dir (:options res))))
    (is (= nil (:errors res)))))

(deftest cli-opts-threshold-not-a-number
  (let [args ["-t" "2a"]
        res (parse-opts args cliOpts)]
    (is (= [] (:arguments res)))
    (is (= false (:linter (:options res))))
    (is (= false (:interactive (:options res))))
    (is (= false (:help (:options res))))
    (is (:errors res))))

(deftest cli-opts-threshold-negative
  (let [args ["-t" "-2"]
        res (parse-opts args cliOpts)]
    (is (= [] (:arguments res)))
    (is (= false (:linter (:options res))))
    (is (= false (:interactive (:options res))))
    (is (= false (:help (:options res))))
    (is (:errors res))))

(deftest cli-opts-threshold-positive
  (let [args ["-t" "2"]
        res (parse-opts args cliOpts)]
    (is (= [] (:arguments res)))
    (is (= false (:linter (:options res))))
    (is (= false (:interactive (:options res))))
    (is (= false (:help (:options res))))
    (is (= 2 (:threshold (:options res))))
    (is (= nil (:errors res)))))

;(defn run [opts]
;  (let [dirname (:dir opts)
;        th (:threshold opts)
;        operation ((keyword (:query opts)) operations)
;        cus (filter not-nil? (cus dirname))]
;    (println "Considering" (.size cus) "Java files")
;    (printOperation operation cus th)))

(deftest run-invoke-the-right-stuff
  (let [opts {:dir "mydir", :threshold 43, :query :mcp}]
    (stubbing [cus '(:cu1 :cu2 :cu3)]
      (mocking [println printOperation]
        (run opts)
        (verify-call-times-for println 1)
        (verify-call-times-for printOperation 1)
        (verify-first-call-args-for println "Considering" 3 "Java files")
        (verify-first-call-args-for printOperation constructorsWithManyParametersOp '(:cu1 :cu2 :cu3) 43)))))

