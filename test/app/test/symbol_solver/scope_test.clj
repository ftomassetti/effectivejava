(ns app.test.symbol_solver.scope_test
  (:use [app.jarloading])
  (:use [app.model.protocols])
  (:use [app.model.javaparser])
  (:use [app.javaparser.navigation])
  (:use [app.symbol_solver.funcs])
  (:use [app.symbol_solver.type_solver])
  (:use [app.symbol_solver.scope])
  (:use [app.utils])
  (:use [clojure.test])
  (:use [app.test.symbol_solver.helper]))

(deftest testSolveNameInVariableDeclarator
  (let [aClassResolvingToLocalVar (sampleClass "AClassResolvingToLocalVar")
        vd (first (getVariableDeclarators aClassResolvingToLocalVar))]
    (is (not (nil? (solveAmongVariableDeclarator "i" vd))))))

(deftest testSolveNameInVariableDeclarationExpr
  (let [aClassResolvingToLocalVar (sampleClass "AClassResolvingToLocalVar")
        vde (first (getVariableDeclarationExprs aClassResolvingToLocalVar))]
    (is (not (nil? (solveSymbol vde nil "i"))))))

(deftest testSolveNameInBlock
  (let [aClassResolvingToLocalVar (sampleClass "AClassResolvingToLocalVar")
        bs (first (getBlockStmts aClassResolvingToLocalVar))]
    (is (not (nil? (solveSymbol bs nil "i"))))))

(deftest testDeclaredFieldResolutionFromClass
  (let [aClass (sampleClass "ReferencesToField")
        _ (assert aClass)
        sym (solveSymbol aClass nil "i")]
    (is (not (nil? sym)))
    (is (not (nil? (getType sym))))
    (is (fieldRef? sym))
    (is (primitive? (getType sym)))
    (is (= "int" (typeName (getType sym))))))

(deftest testDeclaredFieldResolutionFromMethod
  (let [aClass (sampleClass "ReferencesToField")
        _ (assert aClass)
        method (getMethodDeclaration aClass "method1")
        _ (assert method)
        sym (solveSymbol method nil "i")]
    (is (not (nil? sym)))
    (is (not (nil? (getType sym))))
    (is (fieldRef? sym))
    (is (primitive? (getType sym)))
    (is (= "int" (typeName (getType sym))))))

(deftest testDeclaredFieldResolutionFromRef
  (let [aClass (sampleClass "ReferencesToField")
        _ (assert aClass)
        method (getMethodDeclaration aClass "method1")
        _ (assert method)
        refI (getNameExprFor method "i")
        _ (assert refI)
        sym (solveSymbol refI nil "i")]
    (is sym)
    (is (not (nil? (getType sym))))
    (is (fieldRef? sym))
    (is (primitive? (getType sym)))
    (is (= "int" (typeName (getType sym))))))

(deftest testInheritedFieldResolutionFromClass
  ; we should define a classSolver looking in the sampleClasses
  (binding [typeSolver (typeSolverOnList sampleClasses)]
    (let [aClass (sampleClass "ReferencesToFieldExtendingClass")
          method (getMethodDeclaration aClass "method2")
          sym (solveSymbol aClass nil "i")]
      (is sym)
      (is (getType sym))
      (is (fieldRef? sym))
      (is (primitive? (getType sym)))
      (is (= "int" (typeName (getType sym)))))))