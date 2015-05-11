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
  (is (= '(1 2 3 4) (preceedingChildren [1 2 3 4 5] 5)))
  (is (= '(1 2) (preceedingChildren [1 2 3 4 5] 3)))
  (is (= '() (preceedingChildren [1 2 3 4 5] 1))))

(deftest testSolveNameInVariableDeclarator
  (let [aClassResolvingToLocalVar (sampleClass "AClassResolvingToLocalVar")
        vd (first (getVariableDeclarators aClassResolvingToLocalVar))]
    (is (not (nil? (solveSymbol vd nil "i"))))))

(deftest testSolveNameInVariableDeclarationExpr
  (let [aClassResolvingToLocalVar (sampleClass "AClassResolvingToLocalVar")
        vde (first (getVariableDeclarationExprs aClassResolvingToLocalVar))]
    (is (not (nil? (solveSymbol vde nil "i"))))))

(deftest testSolveNameInBlock
  (let [aClassResolvingToLocalVar (sampleClass "AClassResolvingToLocalVar")
        bs (first (getBlockStmts aClassResolvingToLocalVar))]
    (is (not (nil? (solveSymbol bs nil "i"))))))

(deftest testSolveNameExprRefToLocalVar
  (let [aClassResolvingToLocalVar (sampleClass "AClassResolvingToLocalVar")
        method1 (getMethodDeclaration aClassResolvingToLocalVar "method1")
        refI (getNameExprFor method1 "i")]
    (is (not (nil? (solveNameExpr refI))))))

(deftest testTypeCalculationOnLocalVar1
  (let [aClassResolvingToLocalVar (sampleClass "AClassResolvingToLocalVar")
        method1 (getMethodDeclaration aClassResolvingToLocalVar "method1")
        refI (getNameExprFor method1 "i")
        sym (solveNameExpr refI)]
    (is (not (nil? (getType sym))))
    (is (primitive? (getType sym)))
    (is (= "int" (typeName (getType sym))))))

(deftest testTypeCalculationOnLocalVar2
  (let [aClassResolvingToLocalVar (sampleClass "AClassResolvingToLocalVar")
        method2 (getMethodDeclaration aClassResolvingToLocalVar "method2")
        refI (getNameExprFor method2 "i")
        sym (solveNameExpr refI)]
    (is (not (nil? (getType sym))))
    (is (primitive? (getType sym)))
    (is (= "long" (typeName (getType sym))))))