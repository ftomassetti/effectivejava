(ns effectivejava.test.symbol_solver.funcs_test
  (:use [effectivejava.jarloading])
  (:use [effectivejava.model.facade])
  (:use [effectivejava.javaparser.facade])
  (:use [effectivejava.symbol_solver.funcs])
  (:use [effectivejava.symbol_solver.type_solver])
  (:use [effectivejava.symbol_solver.scope])
  (:use [effectivejava.test.symbol_solver.helper])
  (:use [effectivejava.utils])
  (:use [clojure.test]))

; ============================================
; solveNameExpr
; ============================================

; Here we just check if we are able to resolve a simple local var (no other vars with the same name around)
; the variable has primitive type
(deftest testTypeCalculationOnLocalVarPrimitiveType1
  (let [aClassResolvingToLocalVar (sampleClass "AClassResolvingToLocalVar")
        method1 (getMethodDeclaration aClassResolvingToLocalVar "method1")
        refI (getNameExprFor method1 "i")
        sym (solveNameExpr refI)]
    (is sym)
    (is (getType sym))
    (is (localVarRef? sym))
    (is (primitive? (getType sym)))
    (is (= "int" (typeName (getType sym))))))

; Here we check if we are able to resolve a simple local var: there are two local variables with that name,
; we ensure we get the correct one (having long type, the other one has int type)
; the variable has primitive type
(deftest testTypeCalculationOnLocalVarPrimitiveType2
  (let [aClassResolvingToLocalVar (sampleClass "AClassResolvingToLocalVar")
        method2 (getMethodDeclaration aClassResolvingToLocalVar "method2")
        refI (getNameExprFor method2 "i")
        sym (solveNameExpr refI)]
    (is (not (nil? (getType sym))))
    (is (localVarRef? sym))
    (is (primitive? (getType sym)))
    (is (= "long" (typeName (getType sym))))))

; Here we just check if we are able to resolve a simple local var (no other vars with the same name around)
; the variable has reference type
(deftest testTypeCalculationOnLocalVarClassType1
  (let [aClassResolvingToLocalVar (sampleClass "AClassResolvingToLocalVarClassType")
        method1 (getMethodDeclaration aClassResolvingToLocalVar "method1")
        refI (getNameExprFor method1 "i")
        sym (solveNameExpr refI)]
    (is sym)
    (is (getType sym))
    (is (localVarRef? sym))
    (is (not (primitive? (getType sym))))
    (is (= "A" (typeName (getType sym))))))

; Here we check if we are able to resolve a simple local var: there are two local variables with that name,
; we ensure we get the correct one (having long type, the other one has int type)
; the variable has reference type
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

(deftest testTypeCalculationOnReferencesToParameter
  (let [aClass (sampleClass "ReferenceToParameter")
        method (getMethodDeclaration aClass "aMethod")
        ref (getNameExprFor method "foo")
        sym (solveNameExpr ref)]
    (is sym)
    (is (getType sym))
    (is (parameterRef? sym))
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

; ============================================
; solveSuperclass
; ============================================

(deftest testSolveSuperclassSimpleCase
  (binding [typeSolver (typeSolverOnJar javaparser2)]
    (let [scA (sampleClass "SC_A")
          _ (assert scA)
          scB (sampleClass "SC_B")
          _ (assert scB)
          scC (sampleClass "SC_C")
          _ (assert scC)]
      (is (= scB (solveSuperclass scA)))
      (is (= scC (solveSuperclass scB)))
      (is (nil? (solveSuperclass scC))))))

; ============================================
; getAllSuperclasses
; ============================================

(deftest testGetAllSuperclasses
  (binding [typeSolver (typeSolverOnJar javaparser2)]
    (let [scA (sampleClass "SC_A")
          _ (assert scA)
          scB (sampleClass "SC_B")
          _ (assert scB)
          scC (sampleClass "SC_C")
          _ (assert scC)]
      (is (= [scB scC] (getAllSuperclasses scA)))
      (is (= [scC] (getAllSuperclasses scB)))
      (is (= [] (getAllSuperclasses scC))))))