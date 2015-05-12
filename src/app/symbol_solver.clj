(ns app.symbol_solver
  (:use [app.javaparser])
  (:use [app.operations])
  (:use [app.itemsOnLifecycle])
  (:use [app.utils])
  (:require [instaparse.core :as insta])
  (:import [app.operations Operation])
  (:use [app.symbol_solver.type_solver])
  (:use [app.symbol_solver.scope])
  (:use [app.model.protocols]))

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

;
; protocol typedef
;

(extend-protocol TypeDef
  com.github.javaparser.ast.body.ClassOrInterfaceDeclaration
  (allFields [this]
    (let [fields (filter (partial instance? com.github.javaparser.ast.body.FieldDeclaration) (.getMembers this))
          varFields (map (fn [f] (seq (.getVariables f))) fields)]
      (flatten varFields))))

;
; protocol FieldDecl
;

(extend-protocol FieldDecl
  com.github.javaparser.ast.body.VariableDeclarator
  (fieldName [this]
    (.getName (.getId this))))

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
; protocol typeref
;

(extend-protocol TypeRef
  com.github.javaparser.ast.type.PrimitiveType
  (primitive? [this] true)
  (typeName [this] (.toLowerCase (.name (.getType this)))))

(extend-protocol TypeRef
  com.github.javaparser.ast.type.ReferenceType
  (primitive? [this] false)
  (typeName [this]
    (when (nil? (.getType this))
      (throw (IllegalStateException. "No getType for the ReferenceType")))
    (typeName (.getType this))))

(extend-protocol TypeRef
  com.github.javaparser.ast.type.ClassOrInterfaceType
  (primitive? [this] false)
  (typeName [this] (.getName this)))

(defn solveNameExpr [nameExpr]
  ; TODO consider local variables
  ; TODO consider fields
  ; TODO consider inherited fields
  (let [name (.getName nameExpr)]
    (solveSymbol nameExpr nil name)))

(defn solveImportStmt [importStmt]
  (let [name (importQName importStmt)]
    (solveClass (getCu importStmt) nil name)))