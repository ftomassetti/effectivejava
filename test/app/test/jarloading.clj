(ns app.test.jarloading
  (:use [app.jarloading])
  (:use [clojure.test]))

(def javaparser2 "test-resources/sample-jars/javaparser-core-2.0.0.jar")

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