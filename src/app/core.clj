(ns app.core
  (:require [clojure.java.io :as io])
  (:use [clojure.tools.cli :only (cli)])
  (:gen-class :main true))

(import japa.parser.JavaParser)
(import japa.parser.ast.CompilationUnit)
(import japa.parser.ast.body.ClassOrInterfaceDeclaration)
(import japa.parser.ast.body.ConstructorDeclaration)

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

(defn parse [filename]
  (parseFile (new java.io.File filename)))

(defn parseAllFiles [files]
  (map (fn [f] {:file f :cu (parseFile f)}) files))

(defn parseAllDir [dirname]
  (parseAllFiles (java-files dirname)))

(defn parseDir [dirname]
  (let [d (java.io.File. dirname)]
    (doseq [f (.listFiles d)]
      (if (.isDirectory f)
        (print "d ")
        (print "- "))
      (println (.getName f)))))

; ============================================
; Model
; ============================================

(defn isClass? [n]
  (and
    (instance? ClassOrInterfaceDeclaration n)
    (not (.isInterface n))))

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

(defn classQname [cl]
  (let
      [pn (packageName cl),
      cn (.getName cl)]
    (if (.isEmpty pn)
      cn
      (str pn "." cn))))

; TODO Consider internal classes
(defn getClasses [cu]
  (filter isClass? (.getTypes cu)))

; Get tuples of [filename cu]
(defn cusTuples [dirname]
  (filter not-nil? (parseAllDir dirname)))

(defn cus [dirname]
  (map (fn [cuTuple] (:cu cuTuple)) (cusTuples dirname)))

(defn getConstructors [cl]
  (filter (fn [m] (instance? ConstructorDeclaration m)) (.getMembers cl)))

(defn nConstructors [cl]
  (.size (getConstructors cl)))

(defn getParameters [m]
  (let [ps (.getParameters m)]
    (if (nil? ps)
      (java.util.ArrayList. )
      ps)))

; ============================================
; ITEM 1
; ============================================

(defn printClassesWithManyConstructors [cus threshold]
  (doseq [cu cus]
    (doseq [cl (getClasses cu)]
      (let [nc (nConstructors cl)]
        (if (>= nc threshold)
          (println (classQname cl) " : " nc)
          nil)))))

; ============================================
; ITEM 2
; ============================================

(defn printConstructorsWithManyParameters [cus threshold]
  (doseq [cu cus]
    (doseq [cl (getClasses cu)]
      (doseq [cs (getConstructors cl)]
        (let [np (.size (getParameters cs))]
          (if (>= np threshold)
            (println (classQname cl) "." cs " : " np)
            nil))))))

; ============================================
; CLI
; ============================================

(defn name2query [name]
  (cond
    (= "mc" name) printClassesWithManyConstructors
    (= "mcp" name) printConstructorsWithManyParameters
    :else nil))

(defn threshold2number [th]
  (if (number? th)
    th
    (try
      (Integer/parseInt th)
      (catch NumberFormatException e nil))))

(defn run [opts]
  (let [dirname (:dir opts)
        th (threshold2number (:threshold opts))
        query (name2query (:query opts))
        cus (cus dirname)]
    (do
      (println "Considering" (.size cus) "Java files")
      (query cus th))))

(defn -main
  "What I do"
  [& args]
  (let [[opts args banner]
    (cli args
      ["-h" "--help" "Show help" :flag true :default false]
      ["-d" "--dir" "REQUIRED: Directory containing the code to check"]
      ["-q" "--query" "REQUIRED: Query to perform: mc=many constructors, mcp=many constructor parameters"]
      ["-t" "--threshold" "Threshold to be used in the query" :default 0]
      )]
    (when (:help opts)
      (println banner)
      (System/exit 0))
    (if
      (and
        (:dir opts)
        (:query opts)
        (name2query (:query opts))
        (threshold2number (:threshold opts) ))
      (run opts)
      (println banner))))
