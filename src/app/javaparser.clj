(ns app.javaparser
  (:use [app.utils]))

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
(import com.github.javaparser.ast.visitor.DumpVisitor)

; ============================================
; Parsing
; ============================================

(defn parseFile
  "Parse a file, and return nil if the file cannot be loaded"
  [file]
  (try
    (JavaParser/parse file)
    (catch Exception e nil)))

(defn parseString [s]
  (let [reader (java.io.StringReader. s)]
    (try
      (JavaParser/parse reader false)
      (catch Exception e nil))))

(defn parseFileByName [filename]
  (parseFile (new java.io.File filename)))

(defn parseFiles [files]
  (map (fn [f] {:file f :cu (parseFile f)}) files))

(defn parseDirByName [dirname]
  (parseFiles (java-files dirname)))

; ============================================
; Recognize node types
; ============================================

(defn isClass? [n]
  (and
   (instance? ClassOrInterfaceDeclaration n)
   (not (.isInterface n))))

(defn isInterface? [n]
  (and
   (instance? ClassOrInterfaceDeclaration n)
   (.isInterface n)))

(defn isEnum? [n]
  (instance? EnumDeclaration n))

; ============================================
; Modifiers
; ============================================

(defprotocol withModifiers
  (getModifiers [this]))

(extend-protocol withModifiers
  ConstructorDeclaration
  (getModifiers [this] (.getModifiers this)))

(extend-protocol withModifiers
  TypeDeclaration
  (getModifiers [this] (.getModifiers this)))

(extend-protocol withModifiers
  MethodDeclaration
  (getModifiers [this] (.getModifiers this)))

(extend-protocol withModifiers
  FieldDeclaration
  (getModifiers [this] (.getModifiers this)))

(defn isPrivate? [cl]
  (let [ms (getModifiers cl)]
    (ModifierSet/isPrivate ms)))

(defn isPublic? [cl]
  (let [ms (getModifiers cl)]
    (ModifierSet/isPublic ms)))

(defn isProtected? [cl]
  (let [ms (getModifiers cl)]
    (ModifierSet/isProtected ms)))

(defn isStatic? [cl]
  (let [ms (getModifiers cl)]
    (ModifierSet/isStatic ms)))

(defn isFinal? [cl]
  (let [ms (getModifiers cl)]
    (ModifierSet/isFinal ms)))

(defn hasPackageLevelAccess? [cl]
  (not
   (or
    (isPublic? cl)
    (isPrivate? cl)
    (isProtected? cl))))

(defn isPublicOrHasPackageLevelAccess? [el]
  (or
   (isPublic? el)
   (hasPackageLevelAccess? el)))

(defn isNotPrivate? [cl]
  (complement isPrivate?))

; ============================================
; Abstractions
; ============================================

(defrecord SingleFieldDeclaration [field variable]
  withModifiers
  (getModifiers [this] (getModifiers field)))

; ============================================
; Naming
; ============================================

(defprotocol withPackageName
  (packageName [this]))

(extend-protocol withPackageName
  CompilationUnit
  (packageName [this]
    (let
     [p (.getPackage this)]
      (if (nil? p)
        ""
        (str (.getName p))))))

(extend-protocol withPackageName
  Node
  (packageName [this]
    (let [pn (.getParentNode this)]
      (packageName pn))))

(extend-protocol withPackageName
  SingleFieldDeclaration
  (packageName [this]
    (packageName (.variable this))))

(defprotocol Named
  (getName [this])
  (getQName [this]))

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

(defn topLevelTypes [cu]
  (if (nil? cu) []
      (.getTypes cu)))

(defn directlyAnnidatedTypes [t]
  (filter (fn [m] (instance? TypeDeclaration m)) (.getMembers t)))

(defn annidatedTypes
  "Get the types annidated in the given type, recursively"
  [t]
  (flatten
   (map
    (fn [dat] [dat, (directlyAnnidatedTypes dat)])
    (directlyAnnidatedTypes t))))

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

(defn getChildrenNodes [class]
  (tree-seq
   #(not-empty (.getChildrenNodes %))
   #(.getChildrenNodes %)
   class))

(defn getMethodCallExprs [class]
  (filter #(instance? MethodCallExpr %)
          (getChildrenNodes class)))
