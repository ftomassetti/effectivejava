(ns app.symbol_solver.type_solver
  (:use [app.javaparser])
  (:use [app.operations])
  (:use [app.itemsOnLifecycle])
  (:use [app.utils])
  (:require [instaparse.core :as insta])
  (:import [app.operations Operation]))

; A TypeSolver is a function which, given a name, return a TypeRef or nil (if not found)

(defn typeSolverOnList
  "Given a list of com.github.javaparser.ast.body.TypeDeclaration it returns a TypeSolver looking among them"
  [typeDeclarations]
  (fn [nameToSolve]
    (first
      (filter
        (fn [td] (= nameToSolve (getQName td)))
        (remove isInDefaultPackage? typeDeclarations)))))