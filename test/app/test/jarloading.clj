(ns app.test.jarloading
  (:use [app.jarloading])
  (:use [app.model.javaparser])
  (:use [app.javaparser.navigation])
  (:use [clojure.test]))

(def javaparser2 "test-resources/sample-jars/javaparser-core-2.0.0.jar")
(def samplesCus (cus "test-resources/sample-codebases/samples/"))
(def sampleClasses (flatten (map allClasses samplesCus)))

(defn- sampleClass [name]
  (first (filter (fn [c] (= name (.getName c))) sampleClasses)))

(deftest testGetElementsEntriesInJar
  (is (= 137 (count (getElementsEntriesInJar javaparser2)))))

(deftest testGetClassesEntriesInJar
  (is (= 134 (count (getClassesEntriesInJar javaparser2)))))

(deftest testPathToTypeName
  (is (= "com.github.javaparser.ast.AccessSpecifier" (pathToTypeName "com/github/javaparser/ast/AccessSpecifier.class")))
  (is (= "com.github.javaparser.ASTParser.Modifier" (pathToTypeName "com/github/javaparser/ASTParser$Modifier.class"))))

(deftest testFindEntry
  (let [entries (getClassesEntriesInJar javaparser2)]
    (is (nil? (findEntry "com.github.foo.unexisting" entries)))
    (is (not (nil? (findEntry "com.github.javaparser.ast.AccessSpecifier" entries))))
    ; internal class
    (is (not (nil? (findEntry "com.github.javaparser.ASTParser.Modifier" entries))))))

(deftest testFindType
  (let [entries (getClassesEntriesInJar javaparser2)
        ctJavaParser (findType "com.github.javaparser.JavaParser" entries)
        parseBlock (.getDeclaredMethod ctJavaParser "parseBlock")]
    (is (not (.isInterface ctJavaParser)))
    (is (= "parseBlock" (.getName parseBlock)))))

(deftest testSolveSymbol
  (let [entries (getClassesEntriesInJar javaparser2)
        aClassExtendingClassInJar (sampleClass "AClassExtendingClassInJar")
        nameExpr (getNameExprFor aClassExtendingClassInJar "name")]))