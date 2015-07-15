(ns effectivejava.test.symbol_solver.scope_test
  (:use [effectivejava.jarloading])
  (:use [effectivejava.model.protocols])
  (:use [effectivejava.model.javaparser])
  (:use [effectivejava.javaparser.navigation])
  (:use [effectivejava.symbol_solver.funcs])
  (:use [effectivejava.symbol_solver.type_solver])
  (:use [effectivejava.symbol_solver.scope])
  (:use [effectivejava.utils])
  (:use [effectivejava.test.helper])
  (:use [clojure.test])
  (:use [effectivejava.test.symbol_solver.helper]))

(deftest testSolveNameInVariableDeclarator
  (let [aClassResolvingToLocalVar (sampleClass "AClassResolvingToLocalVar")
        vd (first (getVariableDeclarators aClassResolvingToLocalVar))]
    (is (solveAmongVariableDeclarator "i" vd))))

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

; ============================================
; solveSymbol for MethodDeclaration
; ============================================

(deftest test-solveSymbol-for-MethodDeclaration-among-parameters
  (let [aClass (sampleClass "ReferenceToParameter")
        _ (assert aClass)
        method (getMethodDeclaration aClass "aMethod")
        _ (assert method)
        decl (solveSymbol method nil "foo")]
    (is decl)
    (is (instance? com.github.javaparser.ast.body.Parameter decl))))

; ============================================
; solve-among-parameters
; ============================================

(deftest test-solve-among-parameters
  (let [aClass (sampleClass "ReferenceToParameter")
        _ (assert aClass)
        method (getMethodDeclaration aClass "aMethod")
        _ (assert method)
        decl (solve-among-parameters method "foo")]
    (is decl)
    (is (instance? com.github.javaparser.ast.body.Parameter decl))))

; ============================================
; solve class name
; ============================================

(def test-solve-class-name-referring-itself
  (let [cu (parseResource "TypesToMatch")
        c (first (allTypes cu))
        type-a (.getType (.get (.getMembers c) 0))
        type-array-a (.getType (.get (.getMembers c) 1))
        type-qname-a (.getType (.get (.getMembers c) 2))
        type-array-qname-a (.getType (.get (.getMembers c) 3))]
    (binding [typeSolver (jreTypeSolver)]
      (is (= c (solveClass type-a nil "A")))
      (is (= c (solveClass type-qname-a nil "com.foo.A"))))))

(deftest test-solveClass-in-java-lang
  (binding [typeSolver (jreTypeSolver)]
    (is (solveClass nil nil "Object"))
    (is (solveClass nil nil "java.lang.Object"))))