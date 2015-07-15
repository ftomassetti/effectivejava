(ns effectivejava.test.acceptance
  (:use [effectivejava.model.javaparser])
  (:use [effectivejava.operations])
  (:use [effectivejava.itemsOnLifecycle])
  (:use [effectivejava.javaparser.parsing])
  (:use [effectivejava.javaparser.navigation])
  (:use [clojure.test]))

(def javaparserCus (cus "test-resources/sample-codebases/javaparser/"))
(def springJdbcCus (cus "test-resources/sample-codebases/spring-jdbc/"))

(defn toPlain [res]
  (set (map (fn [entry]
              (vec (map
                    toString
                    entry))) res)))

(deftest testParseFileByNameOnAccessSpecifier
  (is (not (nil? (parseFileByName "test-resources/sample-codebases/javaparser/japa/parser/ast/Node.java")))))

(deftest testAllClassesOnAccessSpecifier
  (is (= 1 (count (allClasses (parseFileByName "test-resources/sample-codebases/javaparser/japa/parser/ast/Node.java"))))))

(deftest testAllClassesForCusOnJavaParser
  (is (= 106 (count (allClassesForCus javaparserCus)))))

(deftest testClassesWithManyConstructorOnJavaParser
  (let [res (classesWithManyConstructors {:cus javaparserCus, :threshold 5})
        pres (toPlain res)]
    (is (= 4 (count res)))
    (is (= #{["japa.parser.ast.body.BaseParameter" "5"]
             ["japa.parser.ast.body.FieldDeclaration" "5"]
             ["japa.parser.ast.body.MethodDeclaration" "5"]
             ["japa.parser.ast.expr.ArrayCreationExpr" "5"]} pres))))

(deftest testConstructorsWithManyParametersOnJavaParser
  (let [res (constructorsWithManyParameters {:cus javaparserCus, :threshold 12})
        pres (toPlain res)]
    (is (= #{["japa.parser.ast.body.ClassOrInterfaceDeclaration.ClassOrInterfaceDeclaration(final int beginLine, final int beginColumn, final int endLine, final int endColumn, final int modifiers, final List<AnnotationExpr> annotations, final boolean isInterface, final String name, final List<TypeParameter> typeParameters, final List<ClassOrInterfaceType> extendsList, final List<ClassOrInterfaceType> implementsList, final List<BodyDeclaration> members)" "12"] 
             ["japa.parser.ast.body.MethodDeclaration.MethodDeclaration(final int beginLine, final int beginColumn, final int endLine, final int endColumn, final int modifiers, final List<AnnotationExpr> annotations, final List<TypeParameter> typeParameters, final Type type, final String name, final List<Parameter> parameters, final int arrayCount, final List<NameExpr> throws_, final BlockStmt block)" "13"]} pres))))

(deftest testClassesAndSingletonTypeOnSpringJbc
  (let [res (classesAndSingletonType {:cus springJdbcCus})
        pres (toPlain res)]
    (is (= #{["org.springframework.jdbc.core.namedparam.EmptySqlParameterSource" "publicField"]
             ["org.springframework.jdbc.support.CustomSQLExceptionTranslatorRegistry" "staticFactory"]
             ["org.springframework.jdbc.support.SQLErrorCodesFactory" "staticFactory"]
             ["org.springframework.jdbc.datasource.embedded.HsqlEmbeddedDatabaseConfigurer" "staticFactory"]
             ["org.springframework.jdbc.datasource.embedded.H2EmbeddedDatabaseConfigurer" "staticFactory"]
             ["org.springframework.jdbc.datasource.embedded.DerbyEmbeddedDatabaseConfigurer" "staticFactory"]} pres))))
