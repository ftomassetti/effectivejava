(ns app.test.itemsOnLifecycle
  (:use [app.core])
  (:use [app.operations])
  (:use [app.itemsOnLifecycle])
  (:use [app.javaparser])
  (:use [clojure.test])
  (:use [app.test.helper])
  (:require [instaparse.core :as insta]))

(def javaparserCus (cus "test-resources/sample-codebases/javaparser/"))

(deftest testParseFileByNameOnAccessSpecifier
  (let [clazzes (flatten (map allClasses javaparserCus))]
    (is (= 
         #{"japa.parser.JavaParser" "japa.parser.ast.body.ModifierSet" "japa.parser.ASTHelper" "japa.parser.SourcesHelper" "japa.parser.PositionUtils"}
         (set (map getQName (filter isUtilClass? clazzes)))))))

