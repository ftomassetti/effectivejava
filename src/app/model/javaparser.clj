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
      cn (getName this)]
      (if (.isEmpty pn)
        cn
        (str pn "." cn)))))

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

(defn allTypes
  "Get all the types in the Compilation Unit includin both top level types and annidated types"
  [cu]
  (flatten
   (map
    (fn [t] [t, (annidatedTypes t)])
    (topLevelTypes cu))))

(defn allClasses [cu]
  (filter isClass? (allTypes cu)))

(defn allInterfaces [cu]
  (filter isInterface? (.getTypes cu)))

(defn allEnums [cu]
  (filter isEnum? (.getTypes cu)))

(defn allClassesForCus [cus]
  (flatten
   (for [cu cus]
     (allClasses cu))))

(defn allClassesForCusTuples [cusTuples]
  (flatten
   (for [cuTuple cusTuples]
     (allClasses (:cu cuTuple)))))

(defn cusTuples "Get tuples of [filename cu]" [dirname]
  (filter not-nil? (parseDirByName dirname)))

(defn cus [dirname]
  (map :cu (cusTuples dirname)))

(defn getConstructors [cl]
  (filter (fn [m] (instance? ConstructorDeclaration m)) (.getMembers cl)))

(defn allConstructorsForCus [cus]
  (flatten
   (for [cl (allClassesForCus cus)]
     (getConstructors cl))))

(defn getMethods [cl]
  (filter (fn [m] (instance? MethodDeclaration m)) (.getMembers cl)))

(defn getFields [cl]
  (filter (fn [m] (instance? FieldDeclaration m)) (.getMembers cl)))

(defn getFieldsVariablesTuples [cl]
  (flatten
   (for [f (getFields cl)]
     (for [v (.getVariables f)]
       (SingleFieldDeclaration. f v)))))

(defn getNotPrivateConstructors [cl]
  (filter isNotPrivate? (getConstructors cl)))

(defn nConstructors [cl]
  (.size (getConstructors cl)))

(defn nNotPrivateConstructors [cl]
  (.size (getNotPrivateConstructors cl)))

(defn getParameters [m]
  (let [ps (.getParameters m)]
    (if (nil? ps)
      (java.util.ArrayList.)
      ps)))

(defn getChildrenNodes [root]
  (tree-seq
   #(not-empty (.getChildrenNodes %))
   #(.getChildrenNodes %)
   root))

(defn getMethodCallExprs [root]
  (filter #(instance? MethodCallExpr %)
          (getChildrenNodes root)))

(defn getNameExprs [root]
  (filter #(instance? NameExpr %)
          (getChildrenNodes root)))

(defn getMethodDeclarations [root]
  (filter #(instance? MethodDeclaration %)
          (getChildrenNodes root)))

(defn getBlockStmts [root]
  (filter #(instance? BlockStmt %)
          (getChildrenNodes root)))

(defn getNameExprFor [root name]
  {:pre [root name]}
  (first (filter (fn [ne] (= name (.getName ne))) (getNameExprs root))))

(defn getMethodDeclaration [root name]
  {:pre  [root]}
  (first (filter (fn [ne] (= name (.getName ne))) (getMethodDeclarations root))))

(defn getVariableDeclarationExprs [root]
  (filter #(instance? VariableDeclarationExpr %)
          (getChildrenNodes root)))

(defn getVariableDeclarators [root]
  (filter #(instance? VariableDeclarator %)
          (getChildrenNodes root)))

(defn getImports [root]
  (filter #(instance? com.github.javaparser.ast.ImportDeclaration %)
    (getChildrenNodes root)))

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