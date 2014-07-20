(ns app.core
  (:require [clojure.java.io :as io])
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:gen-class :main true))

(import japa.parser.JavaParser)
(import japa.parser.ast.CompilationUnit)
(import japa.parser.ast.body.ClassOrInterfaceDeclaration)
(import japa.parser.ast.body.EnumDeclaration)
(import japa.parser.ast.body.ConstructorDeclaration)
(import japa.parser.ast.body.FieldDeclaration)
(import japa.parser.ast.body.MethodDeclaration)
(import japa.parser.ast.body.ModifierSet)

; ============================================
; Misc
; ============================================

(def not-nil? (complement nil?))

; ============================================
; Files
; ============================================

(defn java-file? [f]
  (and (.isFile f)
    (.endsWith (.getName f) ".java")))

(defn java-files [dirname]
  (let [d (java.io.File. dirname)]
    (filter java-file? (file-seq d))))

; ============================================
; Parsing
; ============================================

(defn parseFile [file]
  (try
    (JavaParser/parse file)
    (catch Exception e nil)))

(defn parseString [s]
  (let [reader (java.io.StringReader. s)]
    (try
      (JavaParser/parse reader false)
      (catch Exception e nil))))

(defn parse [filename]
  (parseFile (new java.io.File filename)))

(defn parseAllFiles [files]
  (map (fn [f] {:file f :cu (parseFile f)}) files))

(defn parseDir [dirname]
  (parseAllFiles (java-files dirname)))

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
  (filter not-nil? (parseDir dirname)))

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

; ============================================
; ITEM 1
; ============================================

(defn printClassesWithManyConstructors
  "Print the classes which have threshold or more not private constructors"
  [cus threshold]
  (doseq [cl (getClassesForCus cus)]
    (let [nc (nNotPrivateConstructors cl)]
      (if (>= nc threshold)
        (println (classQname cl) " : " nc)
        nil))))

; ============================================
; ITEM 2
; ============================================

(defn printConstructorsWithManyParameters [cus threshold]
  "Print the not private constructors which takes threshold or more parameters"
  (doseq [cl (getClassesForCus cus)]
    (doseq [cs (getNotPrivateConstructors cl)]
      (let [np (.size (getParameters cs))]
        (if (>= np threshold)
          (println (classQname cl) "." cs " : " np)
          nil)))))

; ============================================
; ITEM 3
; ============================================

(defn isPublicFieldSingleton? [cl]
  (not (empty?
         (filter
           (fn [f]
             (and
               (isPublicOrHasPackageLevelAccess? f)
               (isStatic? f)
               (= (getName f) "INSTANCE")))
           (getFieldsVariablesTuples cl)))))

(defn isPublicMethodSingleton? [cl]
  (not (empty?
         (filter
           (fn [m]
             (and
               (isPublicOrHasPackageLevelAccess? m)
               (isStatic? m)
               (= (getName m) "getInstance")))
           (getMethods cl)))))

(defn isSingletonEnum? [e]
  (and
    (=
      1
      (.size
        (.getEntries e)))
    (=
      "INSTANCE"
      (getName
        (first
          (.getEntries e))))))

(defn getSingletonType
  "Return the singleton type or nil: :publicField :getInstance "
  [t]
  (cond
    (and
      (isClass? t)
      (isPublicFieldSingleton? t)) :publicField
    (and
      (isClass? t)
      (isPublicMethodSingleton? t)) :staticFactory
    (and
      (isEnum? t)
      (isSingletonEnum? t)) :singletonEnum
    :else nil))

(defn printSingletonType [cus threshold]
  "Print the not private constructors which takes threshold or more parameters"
  (doseq [t (.getTypes cus)]
    (let [st (getSingletonType t)]
      (if (not-nil? st)
        (println (classQname t) " : " st)
        nil))))

; ============================================
; CLI
; ============================================

(defn name2query [name]
  (cond
    (= "mc" name) printClassesWithManyConstructors
    (= "mcp" name) printConstructorsWithManyParameters
    (= "st" name) printSingletonType
    :else nil))

(defn run [opts]
  (let [dirname (:dir opts)
        th (:threshold opts)
        query (name2query (:query opts))
        cus (cus dirname)]
    (do
      (println "Considering" (.size cus) "Java files")
      (query cus th))))

(defn -main
  "What I do"
  [& args]
  (let [optsMap
    (parse-opts args
      [
        ["-h" "--help" "Show help" :flag true :default false]
        ["-d" "--dir DIRNAME" "REQUIRED: Directory containing the code to check"]
        ["-q" "--query QUERYNAME" "REQUIRED: Query to perform: mc=many constructors, mcp=many constructor parameters, st=singleton type"]
        ["-t" "--threshold VALUE" "Threshold to be used in the query" :default 0
         :parse-fn #(Integer/parseInt %)
         :validate [#(>= % 0) "Must be a number equal or greater to 0"]]
      ])
    opts (:options optsMap)
    banner (:summary optsMap)]
    (when (:help opts)
      (do
        (println ("Printing help message, as asked"))
        (println banner))
      (System/exit 0))
    (if
      (and
        (:dir opts)
        (:query opts)
        (name2query (:query opts))
        (nil? (:errors opts)))
      (run opts)
      (do
        (println "Incorrect usage")
        (when (:errors opts)
          (doseq [e (:errors opts)]
            (println " * " e)))
        (println banner)))))
