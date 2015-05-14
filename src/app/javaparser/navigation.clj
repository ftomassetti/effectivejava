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
  "List of toplevel types: classes, interfaces and enums not contained in other types"
  (if (nil? cu) []
      (.getTypes cu)))

(defn- directlyAnnidatedTypes [t]
  (filter (fn [m] (instance? TypeDeclaration m)) (.getMembers t)))

(defn- annidatedTypes
  "Get the types annidated in the given type, recursively"
  [t]
  (flatten
   (map
    (fn [dat] [dat, (annidatedTypes dat)])
    (directlyAnnidatedTypes t))))

(defn allTypes
  "Get all the types in the Compilation Unit includin both top level types and annidated types"
  [cu]
  (flatten
   (map
    (fn [t] [t, (annidatedTypes t)])
    (topLevelTypes cu))))

(defn allClasses 
  "All non-anonymous classes defined in the CU, including annidated types at all levels"
  [cu]
  (filter isClass? (allTypes cu)))

(defn allInterfaces [cu]
  "All non-anonymous interfaces defined in the CU, including annidated types at all levels"
  (filter isInterface? (allTypes cu)))

(defn allEnums [cu]
  "All non-anonymous enums defined in the CU, including annidated types at all levels"
  (filter isEnum? (allTypes cu)))

(defn allClassesForCus 
  "Get all classes, at all levels of annidation for all the given compilation units"
  [cus]
  (flatten (map allClasses cus)))

(defn- cusTuples "Get tuples of [filename cu]" [dirname]
  (filter not-nil? (parseDirByName dirname)))

(defn cus [dirname]
  (map :cu (cusTuples dirname)))

(defn getConstructors [cl]
  (filter (fn [m] (instance? ConstructorDeclaration m)) (.getMembers cl)))

(defn allConstructorsForCus [cus]
  (flatten
   (for [cl (allClassesForCus cus)]
     (getConstructors cl))))

(defn getMethods [cl]
  (filter (fn [m] (instance? MethodDeclaration m)) (.getMembers cl)))

(defn getFields [cl]
  (filter (fn [m] (instance? FieldDeclaration m)) (.getMembers cl)))

(defn getNotPrivateConstructors [cl]
  (filter isNotPrivate? (getConstructors cl)))

(defn nConstructors [cl]
  (.size (getConstructors cl)))

(defn nNotPrivateConstructors [cl]
  (.size (getNotPrivateConstructors cl)))

(defn getParameters [m]
  (let [ps (.getParameters m)]
    (if (nil? ps)
      (java.util.ArrayList.)
      ps)))

(defn getChildrenNodes [root]
  (tree-seq
   #(not-empty (.getChildrenNodes %))
   #(.getChildrenNodes %)
   root))

(defn getMethodCallExprs [root]
  (filter #(instance? MethodCallExpr %)
          (getChildrenNodes root)))

(defn getNameExprs [root]
  (filter #(instance? NameExpr %)
          (getChildrenNodes root)))

(defn getMethodDeclarations [root]
  (filter #(instance? MethodDeclaration %)
          (getChildrenNodes root)))

(defn getBlockStmts [root]
  (filter #(instance? BlockStmt %)
          (getChildrenNodes root)))

(defn getNameExprFor [root name]
  {:pre [root name]}
  (first (filter (fn [ne] (= name (.getName ne))) (getNameExprs root))))

(defn getMethodDeclaration [root name]
  {:pre  [root]}
  (first (filter (fn [ne] (= name (.getName ne))) (getMethodDeclarations root))))

(defn getVariableDeclarationExprs [root]
  (filter #(instance? VariableDeclarationExpr %)
          (getChildrenNodes root)))

(defn getVariableDeclarators [root]
  (filter #(instance? VariableDeclarator %)
          (getChildrenNodes root)))

(defn getImports [root]
  (filter #(instance? com.github.javaparser.ast.ImportDeclaration %)
          (getChildrenNodes root)))
