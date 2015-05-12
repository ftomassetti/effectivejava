(ns app.symbol_solver.funcs
  (:use [app.model.protocols])
  (:use [app.model.javaparser])
  (:use [app.javaparser.navigation])
  (:use [app.utils])
  (:use [app.symbol_solver.scope]))


(defn solveNameExpr [nameExpr]
  ; TODO consider local variables
  ; TODO consider fields
  ; TODO consider inherited fields
  (let [name (.getName nameExpr)]
    (solveSymbol nameExpr nil name)))

(defn solveImportStmt [importStmt]
  (let [name (importQName importStmt)]
    (solveClass (getCu importStmt) nil name)))