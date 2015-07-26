(ns effectivejava.symbol_solver.facade
  (:use [potemkin])
  (:import [effectivejava.symbol_solver.funcs])
  (:import [effectivejava.symbol_solver.scope])
  (:import [effectivejava.symbol_solver.type_solver]))

;(import-vars [effectivejava.model.protocols isClass? isInterface? isEnum? isNotPrivate? getName getQName packageName allFields fieldName isInDefaultPackage? array? primitive? reference-type? typeName context baseType isPublicOrHasPackageLevelAccess? isStatic? isPrivate? isAbstract? getType localVarRef? fieldRef? parameterRef?])
(import-vars [effectivejava.symbol_solver.scope solveSymbol solveClass solveAmongVariableDeclarator solve-among-parameters])
(import-vars [effectivejava.symbol_solver.funcs solveNameExpr solveImportStmt solveSuperclass getAllSuperclasses])
(import-vars [effectivejava.symbol_solver.type_solver typeSolver typeSolverOnJar typeSolverOnList jreTypeSolver])
