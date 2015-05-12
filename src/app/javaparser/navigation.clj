(ns app.javaparser.navigation
  (:use [app.utils])
  (:use [app.model.protocols])
  (:use [app.javaparser.parsing]))

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
(import com.github.javaparser.ast.expr.NameExpr)
(import com.github.javaparser.ast.visitor.DumpVisitor)
(import com.github.javaparser.ast.stmt.BlockStmt)
(import com.github.javaparser.ast.expr.VariableDeclarationExpr)
(import com.github.javaparser.ast.body.VariableDeclarator)
(import com.github.javaparser.ast.body.VariableDeclaratorId)

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