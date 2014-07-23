(import japa.parser.JavaParser)
(import japa.parser.ast.CompilationUnit)
(import japa.parser.ast.Node)
(import japa.parser.ast.body.ClassOrInterfaceDeclaration)
(import japa.parser.ast.body.EnumDeclaration)
(import japa.parser.ast.body.ConstructorDeclaration)
(import japa.parser.ast.body.FieldDeclaration)
(import japa.parser.ast.body.MethodDeclaration)
(import japa.parser.ast.body.TypeDeclaration)
(import japa.parser.ast.body.ModifierSet)

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
; Model
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
        (.toString (.getName p))))))

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
  (getName [this]))

(extend-protocol Named
  TypeDeclaration
  (getName [this]
    (.getName this)))

(extend-protocol Named
  SingleFieldDeclaration
  (getName [this]
    (getName (.getId (.variable this)))))

(defn getName [el]
  (if
    (map? el)
    (.getName (.getId (:variable el)))
    (.getName el)))

(defn typeQname [cl]
  (let
    [pn (packageName cl),
     cn (.getName cl)]
    (if (.isEmpty pn)
      cn
      (str pn "." cn))))

; TODO Consider internal classes
(defn getClasses [cu]
  (filter isClass? (.getTypes cu)))

(defn getEnums [cu]
  (filter isEnum? (.getTypes cu)))

(defn getClassesForCus [cus]
  (flatten
    (for [cu cus]
      (getClasses cu))))

(defn getClassesForCusTuples [cusTuples]
  (flatten
    (for [cuTuple cusTuples]
      (getClasses (:cu cuTuple)))))

; Get tuples of [filename cu]
(defn cusTuples [dirname]
  (filter not-nil? (parseDirByName dirname)))

(defn cus [dirname]
  (map (fn [cuTuple] (:cu cuTuple)) (cusTuples dirname)))

(defn getConstructors [cl]
  (filter (fn [m] (instance? ConstructorDeclaration m)) (.getMembers cl)))

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
      (java.util.ArrayList. )
      ps)))
