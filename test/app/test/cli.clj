(ns app.test.cli
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:use [app.cli])
  (:use [app.core])
  (:use [app.model.protocols])
  (:use [app.model.javaparser])
  (:use [app.javaparser.navigation])
  (:use [app.operations])
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
