(ns effectivejava.symbol_solver.scope
  (:use [effectivejava.model.protocols])
  (:use [effectivejava.model.javaparser])
  (:use [effectivejava.javaparser.navigation])
  (:use [effectivejava.operations])
  (:use [effectivejava.utils])
  (:use [effectivejava.symbol_solver.type_solver])
  (:require [instaparse.core :as insta])
  (:import [effectivejava.operations Operation]))

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
(import com.github.javaparser.ast.expr.NameExpr)
(import com.github.javaparser.ast.expr.IntegerLiteralExpr)
(import com.github.javaparser.ast.expr.AssignExpr)
(import com.github.javaparser.ast.expr.VariableDeclarationExpr)
(import com.github.javaparser.ast.body.VariableDeclarator)
(import com.github.javaparser.ast.body.VariableDeclaratorId)
(import com.github.javaparser.ast.visitor.DumpVisitor)
(import com.github.javaparser.ast.type.PrimitiveType)

; ==========================================================
; SymbolsDeclarator: we define which nodes are declarations
; ==========================================================

(defprotocol SymbolsDeclarator
  ; Return a map where the keys are the element declared and the values are the declarations
  (declared-symbols [this]))

(extend-protocol SymbolsDeclarator
  com.github.javaparser.ast.Node
  (declared-symbols [this] {}))

(extend-protocol SymbolsDeclarator
  com.github.javaparser.ast.stmt.ExpressionStmt
  (declared-symbols [this]
    (declared-symbols (.getExpression this))))

(extend-protocol SymbolsDeclarator
  com.github.javaparser.ast.expr.VariableDeclarationExpr
  (declared-symbols [this]
    (let [ds (map (fn [v] (declared-symbols v)) (.getVars this))]
      (into {} ds))))

(extend-protocol SymbolsDeclarator
  com.github.javaparser.ast.body.VariableDeclarator
  (declared-symbols [this]
      {(.getName (.getId this)) (.getId this)}))

; ================================================
; scope: we define which declarations are visible
; ================================================

(defprotocol Scope
  ; for example in a BlockStmt containing statements [a b c d e], when solving symbols in the context of c
  ; it will contains only statements preceeding it [a b]
  (solveSymbol [this context nameToSolve])
  ; solveClass solve on a subset of the elements of solveSymbol
  (solveClass [this context nameToSolve]))

(extend-protocol Scope
  nil
  (solveClass [this context nameToSolve] (typeSolver nameToSolve)))

(defn declare-symbol? [symbol-name symbols-declarator]
  (get (declared-symbols symbols-declarator) symbol-name))

(extend-protocol Scope
  com.github.javaparser.ast.Node
  (solveSymbol [this context nameToSolve]
    (solveSymbol (.getParentNode this) this nameToSolve))
  (solveClass [this context nameToSolve]
    (solveClass (.getParentNode this) this nameToSolve)))

(extend-protocol Scope
  BlockStmt
  (solveSymbol [this context nameToSolve]
    (let [elementsToConsider (if (nil? context) (.getStmts this) (preceedingChildren (.getStmts this) context))
          decls (map (partial declare-symbol? nameToSolve) elementsToConsider)]
      (or (first decls) (solveSymbol (.getParentNode this) this nameToSolve)))))

(defn solveClassInPackage [pakage nameToSolve]
  {:pre [typeSolver]}
  (let [qualified-name (if pakage (str (packageName pakage) "." nameToSolve) nameToSolve)]
    (typeSolver qualified-name)))

(defn solveAmongVariableDeclarator
  [nameToSolve variableDeclarator]
  (let [id (.getId variableDeclarator)]
    (when (= nameToSolve (.getName id))
      id)))

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

(extend-protocol Scope
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
    (solveClass (.getParentNode this) nil nameToSolve)))

(defn solve-among-parameters [method nameToSolve]
  (let [parameters (.getParameters method)
        matchingParameters (filter (fn [p] (= nameToSolve (.getName (.getId p)))) parameters)]
    (first matchingParameters)))

(extend-protocol Scope
  com.github.javaparser.ast.body.MethodDeclaration
  (solveSymbol [this context nameToSolve]
    (or (solve-among-parameters this nameToSolve)
      (solveSymbol (.getParentNode this) nil nameToSolve)))
  (solveClass [this context nameToSolve]
    (solveClass (.getParentNode this) nil nameToSolve)))

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
        importNames (map (fn [i] (.getName (.getName i))) imports)
        correspondingClasses (map typeSolver importNames)]
    (first correspondingClasses)))

(extend-protocol Scope
  com.github.javaparser.ast.CompilationUnit
  ; TODO consider imports
  (solveClass [this context nameToSolve]
    (let [typesInCu (topLevelTypes this)
          ; match types in cu using their simple name
          compatibleTypes (filter (fn [t] (= nameToSolve (getName t))) typesInCu)
          ; match types in cu using their qualified name
          compatibleTypes' (filter (fn [t] (= nameToSolve (getQName t))) typesInCu)]
      (or (first compatibleTypes)
          (first compatibleTypes')
          (solveImportedClass this nameToSolve)
          (solveClassInPackage (getClassPackage this) nameToSolve)
          ; we solve in nil context: it means look for absolute names
          (solveClass nil nil nameToSolve)))))
