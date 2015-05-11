(ns app.symbol_solver
  (:use [app.javaparser])
  (:use [app.operations])
  (:use [app.itemsOnLifecycle])
  (:use [app.utils])
  (:require [instaparse.core :as insta])
  (:import [app.operations Operation]))

(import com.github.javaparser.JavaParser)
(import com.github.javaparser.ast.CompilationUnit)
(import com.github.javaparser.ast.Node)
(import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration)
(import com.github.javaparser.ast.body.EnumDeclaration)
(import com.github.javaparser.ast.body.EnumConstantDeclaration)
(import com.github.javaparser.ast.body.ConstructorDeclaration)
(import com.github.javaparser.ast.body.FieldDeclaration)
(import com.github.javaparser.ast.body.MethodDeclaration)
(import com.github.javaparser.ast.body.ModifierSet)
(import com.github.javaparser.ast.body.TypeDeclaration)
(import com.github.javaparser.ast.body.VariableDeclaratorId)
(import com.github.javaparser.ast.stmt.ExpressionStmt)
(import com.github.javaparser.ast.stmt.BlockStmt)
(import com.github.javaparser.ast.expr.MethodCallExpr)
(import com.github.javaparser.ast.expr.NameExpr)
(import com.github.javaparser.ast.expr.IntegerLiteralExpr)
(import com.github.javaparser.ast.expr.AssignExpr)
(import com.github.javaparser.ast.expr.VariableDeclarationExpr)
(import com.github.javaparser.ast.body.VariableDeclarator)
(import com.github.javaparser.ast.body.VariableDeclaratorId)
(import com.github.javaparser.ast.visitor.DumpVisitor)
(import com.github.javaparser.ast.type.PrimitiveType)

(defprotocol typeref
  (array? [this])
  (primitive? [this])
  (typeName [this])
  (baseType [this])
  (allFields [this]))

(defprotocol fieldDecl
  (fieldName [this]))

;
; protocol scope
;

(defprotocol scope
  ; for example in a BlockStmt containing statements [a b c d e], when solving symbols in the context of c
  ; it will contains only statements preceeding it [a b]
  (solveSymbol [this context nameToSolve])
  (solveClass [this context nameToSolve]))

(defn getCu [node]
  (if (instance? com.github.javaparser.ast.CompilationUnit node)
    node 
    (let [pn (.getParentNode node)]
      (if pn
        (getCu pn)
        (throw (IllegalStateException. "The root is not a CU"))))))

(defn getClassPackage [classDecl]
  (let [cu (getCu classDecl)]
    (.getPackage cu)))

(defn solveClassInPackage [pakage nameToSolve])

(defn- solveAmongDeclaredFields [this nameToSolve]
  (let [members (.getMembers this)
        declaredFields (filter (partial instance? com.github.javaparser.ast.body.FieldDeclaration) members)
        solvedSymbols (map (fn [c] (solveSymbol c nil nameToSolve)) declaredFields)
        solvedSymbols' (remove nil? solvedSymbols)]
    (first solvedSymbols')))

(extend-protocol scope
  com.github.javaparser.ast.body.ClassOrInterfaceDeclaration
  (solveSymbol [this context nameToSolve]
    (let [amongDeclaredFields (solveAmongDeclaredFields this nameToSolve)]
      (if (and (nil? amongDeclaredFields) (not (.isInterface this)) (not (empty? (.getExtends this))))
        (let [superclass (first (.getExtends this))
              superclassName (.getName superclass)
              superclassDecl (solveClass this this superclassName)]
          (if (nil? superclassDecl)
            (throw (RuntimeException. (str "Superclass not solved: " superclassName)))
            (let [inheritedFields (allFields superclassDecl)
              solvedSymbols'' (filter (fn [f] (= nameToSolve (fieldName f))) inheritedFields)]
              (first solvedSymbols''))))
        amongDeclaredFields)))
  (solveClass [this context nameToSolve]
    (solveClassInPackage (getClassPackage this) nameToSolve)))

(extend-protocol scope
  com.github.javaparser.ast.body.FieldDeclaration
  (solveSymbol [this context nameToSolve]
    (let [variables (.getVariables this)
          solvedSymbols (map (fn [c] (solveSymbol c nil nameToSolve)) variables)
          solvedSymbols' (remove nil? solvedSymbols)]
      (first solvedSymbols'))))

(extend-protocol scope
  NameExpr
  (solveSymbol [this context nameToSolve]
    (when-not context
      (solveSymbol (.getParentNode this) nil nameToSolve))))

(extend-protocol scope
  AssignExpr
  (solveSymbol [this context nameToSolve]
    (if context
      (or (solveSymbol (.getTarget this) this nameToSolve) (solveSymbol (.getValue this) this nameToSolve))
      (solveSymbol (.getParentNode this) this nameToSolve))))

(extend-protocol scope
  IntegerLiteralExpr
  (solveSymbol [this context nameToSolve] nil))

(defn find-index [elmts elmt]
  (if (empty? elmts)
    -1
    (if (identical? (first elmts) elmt)
      0
      (let [rest (find-index (rest elmts) elmt)]
        (if (= -1 rest)
          -1
          (inc rest))))))

(defn preceedingChildren [children child]
  (let [i (find-index children child)]
    (if (= -1 i)
      (throw (RuntimeException. "Not found"))
      (take i children))))

(extend-protocol scope
  BlockStmt
  (solveSymbol [this context nameToSolve]
    (let [elementsToConsider (if (nil? context) (.getStmts this) (preceedingChildren (.getStmts this) context))
          solvedSymbols (map (fn [c] (solveSymbol c nil nameToSolve)) elementsToConsider)
          solvedSymbols' (remove nil? solvedSymbols)]
      (first solvedSymbols'))))

(extend-protocol scope
  ExpressionStmt
  (solveSymbol [this context nameToSolve]
    (let [fromExpr (solveSymbol (.getExpression this) this nameToSolve)]
      (or fromExpr (solveSymbol (.getParentNode this) this nameToSolve)))))

(extend-protocol scope
  VariableDeclarationExpr
  (solveSymbol [this context nameToSolve]
    (first (filter (fn [s] (not (nil? (solveSymbol s this nameToSolve)))) (.getVars this)))))

(extend-protocol scope
  VariableDeclarator
  (solveSymbol [this context nameToSolve]
    (solveSymbol (.getId this) nil nameToSolve)))

(extend-protocol scope
  VariableDeclaratorId
  (solveSymbol [this context nameToSolve]
    (when (= nameToSolve (.getName this))
      this)))

;
; protocol symbolref
;

(defprotocol symbolref
  (getType [this])
  (localVarRef? [this])
  (fieldRef? [this]))

(extend-protocol symbolref
  VariableDeclarator
  (getType [this]
    (let [variableDeclarationExpr (.getParentNode this)]
      (or (.getType variableDeclarationExpr) (throw (RuntimeException. "No expr")))))
  (localVarRef? [this] (not (fieldRef? this)))
  (fieldRef? [this] (instance? FieldDeclaration (.getParentNode this))))

(extend-protocol symbolref
  VariableDeclaratorId
  ; the parent should be a VariableDeclarator
  (getType [this]
    (getType (.getParentNode this)))
  (localVarRef? [this] (localVarRef? (.getParentNode this)))
  (fieldRef? [this] (fieldRef? (.getParentNode this))))

;
; protocol type
;

(extend-protocol typeref
  com.github.javaparser.ast.type.PrimitiveType
  (primitive? [this] true)
  (typeName [this] (.toLowerCase (.name (.getType this)))))

(extend-protocol typeref
  com.github.javaparser.ast.type.ReferenceType
  (primitive? [this] false)
  (typeName [this]
    (when (nil? (.getType this))
      (throw (IllegalStateException. "No getType for the ReferenceType")))
    (typeName (.getType this))))

(extend-protocol typeref
  com.github.javaparser.ast.type.ClassOrInterfaceType
  (primitive? [this] false)
  (typeName [this] (.getName this)))


(defn solveNameExpr [nameExpr]
  ; TODO consider local variables
  ; TODO consider fields
  ; TODO consider inherited fields
  (let [name (.getName nameExpr)]
    (solveSymbol nameExpr nil name)))