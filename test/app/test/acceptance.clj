(def javaparserCus (cus "test-resources/sample-codebases/javaparser/"))
(def springJdbcCus (cus "test-resources/sample-codebases/spring-jdbc/"))

(defn toPlain [res]
  (set (map (fn [entry]
     (vec (map
            toString
            entry))) res)))

(deftest testClassesWithManyConstructorOnJavaParser
  (let [res (classesWithManyConstructors {:cus javaparserCus, :threshold 5})
        pres (toPlain res)]
    (is (= #{
             ["japa.parser.ast.expr.ArrayCreationExpr" "5"]
             ["japa.parser.ast.body.MethodDeclaration" "5"]
             ["japa.parser.ast.body.BaseParameter" "5"]
             ["japa.parser.ast.body.FieldDeclaration" "5"]
             } pres))))

(deftest testConstructorsWithManyParametersOnJavaParser
  (let [res (constructorsWithManyParameters {:cus javaparserCus, :threshold 12})
        pres (toPlain res)]
    (is (= #{
             ["japa.parser.ast.body.MethodDeclaration.MethodDeclaration(int, int, int, int, int, List<AnnotationExpr>, List<TypeParameter>, Type, String, List<Parameter>, int, List<NameExpr>, BlockStmt)" "13"]
             ["japa.parser.ast.body.ClassOrInterfaceDeclaration.ClassOrInterfaceDeclaration(int, int, int, int, int, List<AnnotationExpr>, boolean, String, List<TypeParameter>, List<ClassOrInterfaceType>, List<ClassOrInterfaceType>, List<BodyDeclaration>)" "12"]
             } pres))))

(deftest testClassesAndSingletonTypeOnSpringJbc
  (let [res (classesAndSingletonType {:cus springJdbcCus})
        pres (toPlain res)]
    (is (= '#{
              ["org.springframework.jdbc.core.namedparam.EmptySqlParameterSource" "publicField"]
              ["org.springframework.jdbc.support.CustomSQLExceptionTranslatorRegistry" "staticFactory"]
              ["org.springframework.jdbc.support.SQLErrorCodesFactory" "staticFactory"]
              ["org.springframework.jdbc.datasource.embedded.HsqlEmbeddedDatabaseConfigurer" "staticFactory"]
              ["org.springframework.jdbc.datasource.embedded.H2EmbeddedDatabaseConfigurer" "staticFactory"]
              ["org.springframework.jdbc.datasource.embedded.DerbyEmbeddedDatabaseConfigurer" "staticFactory"]
              } pres))))