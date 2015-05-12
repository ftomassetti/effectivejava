(ns app.javaparser.navigation
  (:use [app.utils])
  (:use [app.model.protocols]))

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