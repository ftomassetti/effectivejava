(ns app.test.cli
  (:use [app.cli])
  (:use [app.core])
  (:use [app.model.protocols])
  (:use [app.model.javaparser])
  (:use [app.javaparser.navigation])
  (:use [app.operations])
  (:use [clojure.test]))

(def javaparserCus (cus "test-resources/sample-codebases/javaparser/"))

(deftest issue43
  (let [op (:mcp operations)]
    (with-redefs [println (fn [_] 1)]
      (printOperation op javaparserCus 0))))