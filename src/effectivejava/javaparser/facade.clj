(ns effectivejava.javaparser.facade
  (:use [potemkin])
  (:import [effectivejava.javaparser.navigation])
  (:import [effectivejava.javaparser.parsing]))

(import-vars [effectivejava.javaparser.navigation allTypes allClasses allInterfaces allEnums allClassesForCus cus topLevelTypes getConstructors allConstructorsForCus getMethodDeclaration getNameExprFor getImports getVariableDeclarators getBlockStmts])
(import-vars [effectivejava.javaparser.parsing parseFile parseString parseFileByName])