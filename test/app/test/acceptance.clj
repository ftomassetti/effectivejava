(def javaparserCus (cus "test-resources/sample-codeabses/javaparser/"))

(defn toPlain [res]
  (map (fn [entry]
         (into [] (map
                    (fn [field] (toString field))
                    entry))) res))

(deftest testClassesWithManyConstructorOnJavaParser
  (let [res (classesWithManyConstructors {:cus javaparserCus, :threshold 5})
        pres (toPlain res)]
    (is (= '(
              ["japa.parser.ast.expr.ArrayCreationExpr" "5"]
              ["japa.parser.ast.body.MethodDeclaration" "5"]
              ["japa.parser.ast.body.BaseParameter" "5"]
              ["japa.parser.ast.body.FieldDeclaration" "5"]
              ) pres))))

(deftest testConstructorsWithManyParametersOnJavaParser
  (let [res (constructorsWithManyParameters {:cus javaparserCus, :threshold 12})
        pres (toPlain res)]
    (is (= '(
              ["japa.parser.ast.body.MethodDeclaration.MethodDeclaration(int, int, int, int, int, List<AnnotationExpr>, List<TypeParameter>, Type, String, List<Parameter>, int, List<NameExpr>, BlockStmt)" "13"]
              ["japa.parser.ast.body.ClassOrInterfaceDeclaration.ClassOrInterfaceDeclaration(int, int, int, int, int, List<AnnotationExpr>, boolean, String, List<TypeParameter>, List<ClassOrInterfaceType>, List<ClassOrInterfaceType>, List<BodyDeclaration>)" "12"]
              ) pres))))