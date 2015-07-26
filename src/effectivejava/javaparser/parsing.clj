(ns effectivejava.javaparser.parsing
  (:use [effectivejava.utils]))

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

; ============================================
; Parsing
; ============================================

(defn parseFile
  "Parse a file, and return nil if the file cannot be loaded"
  [file]
  (try
    (JavaParser/parse file)
    (catch Exception e nil)))

(defn parseString [s]
  (let [reader (java.io.StringReader. s)]
    (try
      (JavaParser/parse reader false)
      (catch Exception e nil))))

(defn parseFileByName [filename]
  (parseFile (new java.io.File filename)))

(defn parseFiles [files]
  (map (fn [f] {:file f :cu (parseFile f)}) files))

(defn parseDirByName [dirname]
  (parseFiles (java-files dirname)))