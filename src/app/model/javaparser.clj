(ns app.model.javaparser
  (:use [app.utils])
  (:use [app.model.protocols])
  (:use [app.javaparser.parsing])
  (:use [app.javaparser.navigation]))

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
(import com.github.javaparser.ast.expr.MethodCallExpr)
(import com.github.javaparser.ast.expr.NameExpr)
(import com.github.javaparser.ast.visitor.DumpVisitor)
(import com.github.javaparser.ast.stmt.BlockStmt)
(import com.github.javaparser.ast.expr.VariableDeclarationExpr)
(import com.github.javaparser.ast.body.VariableDeclarator)
(import com.github.javaparser.ast.body.VariableDeclaratorId)

; ============================================
; Recognize node types
; ============================================

(extend-protocol TypeDecl
  Node
  (isClass? [this]
    false)
  (isInterface? [n]
    false)
  (isEnum? [n]
    (instance? EnumDeclaration n)))

(extend-protocol TypeDecl
  com.github.javaparser.ast.body.ClassOrInterfaceDeclaration
  (isClass? [this]
    (not (.isInterface this)))
  (isInterface? [n]
    (.isInterface n))
  (isEnum? [n]
    false)
  (allFields [this]
    (let [fields (filter (partial instance? com.github.javaparser.ast.body.FieldDeclaration) (.getMembers this))
          varFields (map (fn [f] (seq (.getVariables f))) fields)]
      (flatten varFields))))

; ============================================
; TypeRef
; ============================================

(extend-protocol TypeRef
  com.github.javaparser.ast.type.PrimitiveType
  (primitive? [this] true)
  (typeName [this] (.toLowerCase (.name (.getType this)))))

(defn reference-type-base-type
  "Generate a TypeRef corresponding to the base type of this. It expects an array"
  [this array-dims context]
  (if (= 0 array-dims)
    (make-reference-type-ref (typeName this) context)
    (make-array-type-ref (reference-type-base-type this (dec array-dims) context))))

(extend-protocol TypeRef
  com.github.javaparser.ast.type.ReferenceType
  (primitive? [this] false)
  (array? [this] (> (.getArrayCount this) 0))
  (reference-type? [this] (not (array? this)))
  (typeName [this]
    (when (nil? (.getType this))
      (throw (IllegalStateException. "No getType for the ReferenceType")))
    (typeName (.getType this)))
  (baseType [this]
    (when (array? this)
      (reference-type-base-type this (.getArrayCount this) this)))
  (context [this] this))

(extend-protocol TypeRef
  com.github.javaparser.ast.type.ClassOrInterfaceType
  (primitive? [this] false)
  (typeName [this] (.getName this)))

; ============================================
; FieldDecl
; ============================================

(extend-protocol FieldDecl
  com.github.javaparser.ast.body.VariableDeclarator
  (fieldName [this]
    (.getName (.getId this))))

; ============================================
; SymbolRef
; ============================================

(extend-protocol SymbolRef
  VariableDeclarator
  (getType [this]
    (let [variableDeclarationExpr (.getParentNode this)]
      (or (.getType variableDeclarationExpr) (throw (RuntimeException. "No expr")))))
  (localVarRef? [this] (not (fieldRef? this)))
  (fieldRef? [this] (instance? FieldDeclaration (.getParentNode this))))

(extend-protocol SymbolRef
  VariableDeclaratorId
  ; the parent should be a VariableDeclarator
  (getType [this]
    (getType (.getParentNode this)))
  (localVarRef? [this] (localVarRef? (.getParentNode this)))
  (fieldRef? [this] (fieldRef? (.getParentNode this))))

(extend-protocol SymbolRef
  com.github.javaparser.ast.body.Parameter
  (getType [this]
    (.getType this))
  (localVarRef? [this] false)
  (parameterRef? [this] true)
  (fieldRef? [this] false))

; ============================================
; Modifiers
; ============================================

(extend-protocol WithModifiers
  TypeDeclaration
  (isPrivate? [this]
    (ModifierSet/isPrivate (.getModifiers this)))
  (isPublic? [this]
    (ModifierSet/isPublic (.getModifiers this)))
  (isProtected? [this]
    (ModifierSet/isProtected (.getModifiers this)))
  (isStatic? [this]
    (ModifierSet/isStatic (.getModifiers this)))
  (isFinal? [this]
    (ModifierSet/isFinal (.getModifiers this))))

(extend-protocol WithModifiers
  ConstructorDeclaration
  (isPrivate? [this]
    (ModifierSet/isPrivate (.getModifiers this)))
  (isPublic? [this]
    (ModifierSet/isPublic (.getModifiers this)))
  (isProtected? [this]
    (ModifierSet/isProtected (.getModifiers this)))
  (isStatic? [this]
    (ModifierSet/isStatic (.getModifiers this)))
  (isFinal? [this]
    (ModifierSet/isFinal (.getModifiers this))))

(extend-protocol WithModifiers
  MethodDeclaration
  (isPrivate? [this]
    (ModifierSet/isPrivate (.getModifiers this)))
  (isPublic? [this]
    (ModifierSet/isPublic (.getModifiers this)))
  (isProtected? [this]
    (ModifierSet/isProtected (.getModifiers this)))
  (isStatic? [this]
    (ModifierSet/isStatic (.getModifiers this)))
  (isFinal? [this]
    (ModifierSet/isFinal (.getModifiers this))))

(extend-protocol WithModifiers
  FieldDeclaration
  (isPrivate? [this]
    (ModifierSet/isPrivate (.getModifiers this)))
  (isPublic? [this]
    (ModifierSet/isPublic (.getModifiers this)))
  (isProtected? [this]
    (ModifierSet/isProtected (.getModifiers this)))
  (isStatic? [this]
    (ModifierSet/isStatic (.getModifiers this)))
  (isFinal? [this]
    (ModifierSet/isFinal (.getModifiers this))))

; ============================================
; Abstractions
; ============================================

(defrecord SingleFieldDeclaration [field variable]
  WithModifiers
  (isPrivate? [this]
    (isPrivate? field))
  (isPublic? [this]
    (isPublic? field))
  (isProtected? [this]
    (isProtected? field))
  (isStatic? [this]
    (isStatic? field))
  (isFinal? [this]
    (isFinal? field)))

; ============================================
; Naming
; ============================================

(extend-protocol WithPackageName
  CompilationUnit
  (packageName [this]
    (let
     [p (.getPackage this)]
      (if (nil? p)
        ""
        (str (.getName p))))))

(extend-protocol WithPackageName
  Node
  (packageName [this]
    (let [pn (.getParentNode this)]
      (packageName pn))))

(extend-protocol WithPackageName
  SingleFieldDeclaration
  (packageName [this]
    (packageName (.variable this))))

(extend-protocol Named
  TypeDeclaration
  (getName [this]
    (.getName this))
  (getQName [this]
    (let
     [pn (packageName this),
      cn (getName this)
      parent (.getParentNode this)
      prefix (if (instance? TypeDeclaration parent) (str (getQName parent) ".") "")]
      (if (.isEmpty pn)
        (str prefix cn)
        (str prefix pn "." cn)))))

(extend-protocol Named
  EnumConstantDeclaration
  (getName [this]
    (.getName this))
  (getQName [this]
    (let [pn (.getParentNode this)]
      (str (getQName pn) "." (getName this)))))

(extend-protocol Named
  MethodDeclaration
  (getName [this]
    (.getName this))
  (getQName [this]
    (let [pn (.getParentNode this)]
      (str (getQName pn) "." (getName this)))))

(defn toStringWithoutComments [node]
  (str node))

(defn constructorDeclarationAsString [constructor]
  (let [sb (StringBuffer.)]
    (.append sb (.getName constructor))
    (.append sb "(")
    (let [firstParam (first (.getParameters constructor))
          otherParams (rest (.getParameters constructor))]
      (when-not (nil? firstParam)
        (.append sb (toStringWithoutComments (.getType firstParam))))
      (doseq [param otherParams]
        (.append sb ", ")
        (.append sb (toStringWithoutComments (.getType param)))))
    (.append sb ")")
    (str sb)))

(extend-protocol Named
  ConstructorDeclaration
  ; unfortunately there is a bug in JavaParser 2.0.0 and getDeclarationAsString can throw a NPE
  ; when the list of parameters is empty, so we forced to a null list in that case
  (getName [this]
    (do 
      (when (zero? (count (.getParameters this)))
        (.setParameters this []))
      (.getDeclarationAsString this false false)))
  (getQName [this]
    (let [pn (.getParentNode this)]
      (str (getQName pn) "." (getName this)))))

(extend-protocol Named
  VariableDeclaratorId
  (getName [this]
    (.getName this)))

(extend-protocol Named
  SingleFieldDeclaration
  (getName [this]
    (getName (.getId (.variable this)))))

; ============================================
; Accessing nodes
; ============================================

(defn getFieldsVariablesTuples [cl]
  (flatten
   (for [f (getFields cl)]
     (for [v (.getVariables f)]
       (SingleFieldDeclaration. f v)))))

; ============================================
; Misc
; ============================================

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