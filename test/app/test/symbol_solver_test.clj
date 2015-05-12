(ns app.test.symbol_solver_test
  (:use [app.jarloading])
  (:use [app.model.protocols])
  (:use [app.javaparser])
  (:use [app.symbol_solver])
  (:use [app.symbol_solver.type_solver])
  (:use [app.symbol_solver.scope])
  (:use [app.utils])
  (:use [clojure.test]))

(def javaparser2 "test-resources/sample-jars/javaparser-core-2.0.0.jar")
(def samplesCus (cus "test-resources/sample-codebases/samples/"))
(def sampleClasses (flatten (map allTypes samplesCus)))

(defn- sampleClass [name]
  {:post [%]}
  (first (filter (fn [c] (= name (.getName c))) sampleClasses)))

(deftest testPreceedingChildren
  (is (= '(1 2 3 4) (preceedingChildren [1 2 3 4 5] 5)))
  (is (= '(1 2) (preceedingChildren [1 2 3 4 5] 3)))
  (is (= '() (preceedingChildren [1 2 3 4 5] 1))))

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

(deftest testSolveNameExprRefToLocalVar
  (let [aClassResolvingToLocalVar (sampleClass "AClassResolvingToLocalVar")
        method1 (getMethodDeclaration aClassResolvingToLocalVar "method1")
        refI (getNameExprFor method1 "i")]
    (is (not (nil? (solveNameExpr refI))))))

(deftest testTypeCalculationOnLocalVarPrimitiveType1
  (let [aClassResolvingToLocalVar (sampleClass "AClassResolvingToLocalVar")
        method1 (getMethodDeclaration aClassResolvingToLocalVar "method1")
        refI (getNameExprFor method1 "i")
        sym (solveNameExpr refI)]
    (is (not (nil? (getType sym))))
    (is (localVarRef? sym))
    (is (primitive? (getType sym)))
    (is (= "int" (typeName (getType sym))))))

(deftest testTypeCalculationOnLocalVarPrimitiveType2
  (let [aClassResolvingToLocalVar (sampleClass "AClassResolvingToLocalVar")
        method2 (getMethodDeclaration aClassResolvingToLocalVar "method2")
        refI (getNameExprFor method2 "i")
        sym (solveNameExpr refI)]
    (is (not (nil? (getType sym))))
    (is (localVarRef? sym))
    (is (primitive? (getType sym)))
    (is (= "long" (typeName (getType sym))))))

(deftest testTypeCalculationOnLocalVarClassType1
  (let [aClassResolvingToLocalVar (sampleClass "AClassResolvingToLocalVarClassType")
        method1 (getMethodDeclaration aClassResolvingToLocalVar "method1")
        refI (getNameExprFor method1 "i")
        sym (solveNameExpr refI)]
    (is (not (nil? (getType sym))))
    (is (localVarRef? sym))
    (is (not (primitive? (getType sym))))
    (is (= "A" (typeName (getType sym))))))

(deftest testTypeCalculationOnLocalVarClassType2
  (let [aClassResolvingToLocalVar (sampleClass "AClassResolvingToLocalVarClassType")
        method2 (getMethodDeclaration aClassResolvingToLocalVar "method2")
        refI (getNameExprFor method2 "i")
        sym (solveNameExpr refI)]
    (is (not (nil? (getType sym))))
    (is (localVarRef? sym))
    (is (not (primitive? (getType sym))))
    (is (= "B" (typeName (getType sym))))))

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

(deftest testTypeCalculationOnReferencesToDeclaredField
  (let [aClass (sampleClass "ReferencesToField")
        _ (assert aClass)
        method (getMethodDeclaration aClass "method1")
        _ (assert method)
        refI (getNameExprFor method "i")
        _ (assert refI)
        sym (solveNameExpr refI)]
    (is sym)
    (is (not (nil? (getType sym))))
    (is (fieldRef? sym))
    (is (primitive? (getType sym)))
    (is (= "int" (typeName (getType sym))))))

(deftest testTypeCalculationOnReferencesToInheritedField
  (let [aClass (sampleClass "ReferencesToFieldExtendingClass")
        method (getMethodDeclaration aClass "method2")
        refI (getNameExprFor method "i")
        sym (solveNameExpr refI)]
    (is (not (nil? sym)))
    (is (not (nil? (getType sym))))
    (is (fieldRef? sym))
    (is (primitive? (getType sym)))
    (is (= "int" (typeName (getType sym))))))

(deftest testSolveImportStmtFromJar
  (binding [typeSolver (typeSolverOnJar javaparser2)]
    (let [aClass (sampleClass "AClassExtendingClassInJar")
         _ (assert aClass)
         importStmt (first (getImports (getCu aClass)))
         _ (assert importStmt)
         importedType (solveImportStmt importStmt)]
      (is importedType))))

;(deftest testSolveClassImportedFromJar
;  )