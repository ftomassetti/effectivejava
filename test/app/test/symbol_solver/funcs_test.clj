(ns app.test.symbol_solver.funcs_test
  (:use [app.jarloading])
  (:use [app.model.protocols])
  (:use [app.model.javaparser])
  (:use [app.javaparser.navigation])
  (:use [app.symbol_solver.funcs])
  (:use [app.symbol_solver.type_solver])
  (:use [app.symbol_solver.scope])
  (:use [app.test.symbol_solver.helper])
  (:use [app.utils])
  (:use [clojure.test]))

; ============================================
; solveNameExpr
; ============================================

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

; ============================================
; solveImportStmt
; ============================================

(deftest testSolveImportStmtFromJar
  (binding [typeSolver (typeSolverOnJar javaparser2)]
    (let [aClass (sampleClass "AClassExtendingClassInJar")
          _ (assert aClass)
          importStmt (first (getImports (getCu aClass)))
          _ (assert importStmt)
          importedType (solveImportStmt importStmt)]
      (is importedType))))
