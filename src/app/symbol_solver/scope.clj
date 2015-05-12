(ns app.symbol_solver.scope
  (:use [app.model.protocols])
  (:use [app.model.javaparser])
  (:use [app.javaparser.navigation])
  (:use [app.operations])
  (:use [app.itemsOnLifecycle])
  (:use [app.utils])
  (:use [app.symbol_solver.type_solver])
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

(defprotocol scope
  ; for example in a BlockStmt containing statements [a b c d e], when solving symbols in the context of c
  ; it will contains only statements preceeding it [a b]
  (solveSymbol [this context nameToSolve])
  (solveClass [this context nameToSolve]))

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

(extend-protocol scope
  BlockStmt
  (solveSymbol [this context nameToSolve]
    (let [elementsToConsider (if (nil? context) (.getStmts this) (preceedingChildren (.getStmts this) context))
          solvedSymbols (map (fn [c] (solveSymbol c nil nameToSolve)) elementsToConsider)
          solvedSymbols' (remove nil? solvedSymbols)]
      (or (first solvedSymbols') (solveSymbol (.getParentNode this) this nameToSolve)))))

(extend-protocol scope
  ExpressionStmt
  (solveSymbol [this context nameToSolve]
    (let [fromExpr (solveSymbol (.getExpression this) this nameToSolve)]
      (or fromExpr (solveSymbol (.getParentNode this) this nameToSolve)))))

(defn solveClassInPackage [pakage nameToSolve]
  {:pre [typeSolver]}
  ; TODO first look into the package
  (typeSolver nameToSolve))

(defn solveAmongVariableDeclarator
  [nameToSolve variableDeclarator]
  (let [id (.getId variableDeclarator)]
    (when (= nameToSolve (.getName id))
      id)))

(extend-protocol scope
  VariableDeclarationExpr
  (solveSymbol [this context nameToSolve]
    (first (filter (partial solveAmongVariableDeclarator nameToSolve) (.getVars this)))))

(defn- solveAmongFieldDeclaration
  "Consider one single com.github.javaparser.ast.body.FieldDeclaration, which corresponds to possibly multiple fields"
  [fieldDeclaration nameToSolve]
  (let [variables (.getVariables fieldDeclaration)
          solvedSymbols (map (partial solveAmongVariableDeclarator nameToSolve) variables)
          solvedSymbols' (remove nil? solvedSymbols)]
      (first solvedSymbols')))

(defn- solveAmongDeclaredFields [this nameToSolve]
  (let [members (.getMembers this)
        declaredFields (filter (partial instance? com.github.javaparser.ast.body.FieldDeclaration) members)
        solvedSymbols (map (fn [c] (solveAmongFieldDeclaration c nameToSolve)) declaredFields)
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
    (solveClass (getCu this) nil nameToSolve)))

(extend-protocol scope
  com.github.javaparser.ast.body.MethodDeclaration
  (solveSymbol [this context nameToSolve]
    (solveSymbol (.getParentNode this) nil nameToSolve)))

(defn qNameToSimpleName [qualifiedName]
  (last (clojure.string/split qualifiedName #"\.")))

(defn importQName [importDecl]
  (str (.getName importDecl)))

(defn isImportMatchingSimpleName? [simpleName importDecl]
  (= simpleName (qNameToSimpleName (importQName importDecl))))

(defn solveImportedClass 
  "Try to solve the classname by looking among the imported classes"
  [cu nameToSolve]
  (let [imports (.getImports cu)
        relevantImports (filter (partial isImportMatchingSimpleName? nameToSolve) imports)
        correspondingClasses (map typeSolver relevantImports)]
    (first correspondingClasses)))
      
(extend-protocol scope
  com.github.javaparser.ast.CompilationUnit
  ; TODO consider imports
  (solveClass [this context nameToSolve]
    (let [typesInCu (topLevelTypes this)
          compatibleTypes (filter (fn [t] (= nameToSolve (getName t))) typesInCu)]
      (or (first compatibleTypes) 
        (solveImportedClass this nameToSolve)
        (solveClassInPackage (getClassPackage this) nameToSolve)))))
