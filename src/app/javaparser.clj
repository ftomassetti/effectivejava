(import japa.parser.JavaParser)
(import japa.parser.ast.CompilationUnit)
(import japa.parser.ast.body.ClassOrInterfaceDeclaration)
(import japa.parser.ast.body.EnumDeclaration)
(import japa.parser.ast.body.ConstructorDeclaration)
(import japa.parser.ast.body.FieldDeclaration)
(import japa.parser.ast.body.MethodDeclaration)
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
; Model
; ============================================

(defn isClass? [n]
  (and
    (instance? ClassOrInterfaceDeclaration n)
    (not (.isInterface n))))

(defn isEnum? [n]
  (instance? EnumDeclaration n))

(defn packageName [n]
  (if (instance? CompilationUnit n)
    (let
      [p (.getPackage n)]
      (if (nil? p)
        ""
        (.toString (.getName p))))
    (try
      (let [pn (.getParentNode n)]
        (if (nil? pn)
          ""
          (packageName pn)))
      (catch Exception e (str "STR PN " (nil? (.getParentNode n)) e) ))))

(defn getName [el]
  (if
    (map? el)
    (.getName (.getId (:variable el)))
    (.getName el)))

(defn classQname [cl]
  (let
    [pn (packageName cl),
     cn (.getName cl)]
    (if (.isEmpty pn)
      cn
      (str pn "." cn))))

(defn typeQname [t]
  (classQname t))

(defn getModifiers [el]
  (if (map? el)
    (getModifiers (:field el))
    (.getModifiers el)))

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
        {:field f, :variable v}))))

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
