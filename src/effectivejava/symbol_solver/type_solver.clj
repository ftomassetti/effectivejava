(ns effectivejava.symbol_solver.type_solver
  (:use [effectivejava.model.facade])
  (:use [effectivejava.operations])
  (:use [effectivejava.jarloading])
  (:use [effectivejava.utils])
  (:require [instaparse.core :as insta])
  (:import [effectivejava.operations Operation]))

; A TypeSolver is a function which, given a name, return a TypeRef or nil (if not found)

; This is intended to be the global typeSolver, so the typeSolver to resolve absolute names
(def ^:dynamic typeSolver (fn [nameToSolve] (throw (IllegalStateException. "TypeSolver not set"))))

(defn typeSolverOnList
  "Given a list of com.github.javaparser.ast.body.TypeDeclaration it returns a TypeSolver looking among them"
  [typeDeclarations]
  (fn [nameToSolve]
    (first
     (filter
      (fn [td] (= nameToSolve (getQName td)))
      (remove isInDefaultPackage? typeDeclarations)))))

; TODO not solve the classes in default package
(defn typeSolverOnJar
  "Given the path to a Jar it returns a TypeSolver looking into it"
  [jarPath]
  (let [entries (getClassesEntriesInJar jarPath)]
    (fn [nameToSolve]
      (findType nameToSolve entries))))

(defn- solve-system-class [class-qname]
  (let [system-class-loader (ClassLoader/getSystemClassLoader)]
    (try 
      (.loadClass system-class-loader class-qname)
      (catch Exception e nil))))

(defn simple-name? [name]
  (not (.contains name ".")))

(defn jreTypeSolver
  "Solve classes part of the standard libraries. For now unfortunately it uses the standard library where this code is executed, not an arbitrary one"
  []
  (fn [name-to-solve]
    (when (nil? name-to-solve)
      (throw (IllegalArgumentException. "null name received")))
    (when (not (instance? String name-to-solve))
      (throw (IllegalArgumentException. (str "name received is not a string, it is" (class name-to-solve)))))
    (if (.startsWith name-to-solve "java.lang.")
      (solve-system-class name-to-solve)
      (if (simple-name? name-to-solve)
        (solve-system-class (str "java.lang." name-to-solve))
        nil))))

(defn combine-solvers
  [solver-1 solver-2]
  (fn [name-to-solve]
    (let [res1 (solver-1 name-to-solve)
          res2 (when solver-2 (solver-2 name-to-solve))]
      (or res1 res2))))