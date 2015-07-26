(ns effectivejava.javaparser.navigation
  (:use [effectivejava.utils])
  (:use [effectivejava.javaparser.parsing]))

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


(defn javaparser-is-class? [node]
  (and
    (instance? com.github.javaparser.ast.body.ClassOrInterfaceDeclaration node)
    (not (.isInterface node))))

(defn javaparser-is-interface? [node]
  (and
    (instance? com.github.javaparser.ast.body.ClassOrInterfaceDeclaration node)
    (.isInterface node)))

(defn javaparser-is-enum? [node]
  (instance? EnumDeclaration node))

(defn javaparser-is-private? [node]
  (ModifierSet/isPrivate (.getModifiers node)))

(defn javaparser-is-not-private? [node]
  (complement javaparser-is-private?))

; ============================================
; Access type declarations
; ============================================

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
  (filter javaparser-is-class? (allTypes cu)))

(defn allInterfaces [cu]
  "All non-anonymous interfaces defined in the CU, including annidated types at all levels"
  (filter javaparser-is-interface? (allTypes cu)))

(defn allEnums [cu]
  "All non-anonymous enums defined in the CU, including annidated types at all levels"
  (filter javaparser-is-enum? (allTypes cu)))

(defn allClassesForCus 
  "Get all classes, at all levels of annidation for all the given compilation units"
  [cus]
  (flatten (map allClasses cus)))

; ============================================
; Compilation units
; ============================================

(defn- cusTuples "Get tuples of [filename cu]" [dirname]
  (filter not-nil? (parseDirByName dirname)))

(defn cus [dirname]
  (map :cu (cusTuples dirname)))

; ============================================
; Constructors
; ============================================

(defn getConstructors [cl]
  (filter (fn [m] (instance? ConstructorDeclaration m)) (.getMembers cl)))

(defn allConstructorsForCus [cus]
  (flatten
   (for [cl (allClassesForCus cus)]
     (getConstructors cl))))

(defn getNotPrivateConstructors [cl]
  (filter javaparser-is-not-private? (getConstructors cl)))

(defn nConstructors [cl]
  (.size (getConstructors cl)))

(defn nNotPrivateConstructors [cl]
  (.size (getNotPrivateConstructors cl)))

; ============================================
; Methods
; ============================================

(defn getMethods [cl]
  (filter (fn [m] (instance? MethodDeclaration m)) (.getMembers cl)))

; ============================================
; Fields
; ============================================

(defn getFields [cl]
  (filter (fn [m] (instance? FieldDeclaration m)) (.getMembers cl)))

; ============================================
; Nodes
; ============================================

(defn getChildrenNodes [root]
  (tree-seq
   #(not-empty (.getChildrenNodes %))
   #(.getChildrenNodes %)
   root))

(defn getParameters [m]
  (let [ps (.getParameters m)]
    (if (nil? ps)
      (java.util.ArrayList.)
      ps)))

; ============================================
; Get Nodes of a certain type
; ============================================

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

(defn getVariableDeclarationExprs [root]
  (filter #(instance? VariableDeclarationExpr %)
          (getChildrenNodes root)))

(defn getVariableDeclarators [root]
  (filter #(instance? VariableDeclarator %)
          (getChildrenNodes root)))

(defn getImports [root]
  (filter #(instance? com.github.javaparser.ast.ImportDeclaration %)
          (getChildrenNodes root)))

; ============================================
; Misc
; ============================================

(defn getNameExprFor [root name]
  {:pre [root name]}
  (first (filter (fn [ne] (= name (.getName ne))) (getNameExprs root))))

(defn getMethodDeclaration [root name]
  {:pre  [root]}
  (first (filter (fn [ne] (= name (.getName ne))) (getMethodDeclarations root))))

(defn getCu [node]
  (if (instance? com.github.javaparser.ast.CompilationUnit node)
    node
    (let [pn (.getParentNode node)]
      (if pn
        (getCu pn)
        (throw (IllegalStateException. "The root is not a CU"))))))

(defn getClassPackage [classDecl]
  (let [cu (getCu classDecl)]
    (.getPackage cu)))
