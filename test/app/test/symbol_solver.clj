(ns app.test.symbol_solver
  (:use [app.jarloading])
  (:use [app.javaparser])
  (:use [app.symbol_solver])
  (:use [clojure.test]))

(def samplesCus (cus "test-resources/sample-codebases/samples/"))
(def sampleClasses (flatten (map allClasses samplesCus)))

(defn- sampleClass [name]
  (first (filter (fn [c] (= name (.getName c))) sampleClasses)))

(deftest testPreceedingChildren
  (is (= '(1 2 3 4)) (preceedingChildren [1 2 3 4 5] 5))
  (is (= '(1 2)) (preceedingChildren [1 2 3 4 5] 3))
  (is (= '()) (preceedingChildren [1 2 3 4 5] 1)))

(deftest testSolveNameExprRefToLocalVar
  (let [aClassResolvingToLocalVar (sampleClass "AClassResolvingToLocalVar")
        method1 (getMethodDeclaration aClassResolvingToLocalVar "method1")
        refI (getNameExprFor method1 "i")]
    ))