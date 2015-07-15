(ns effectivejava.test.itemsOnLifecycle
  (:use [effectivejava.core])
  (:use [effectivejava.operations])
  (:use [effectivejava.itemsOnLifecycle])
  (:use [effectivejava.model.protocols])
  (:use [effectivejava.model.javaparser])
  (:use [effectivejava.javaparser.facade])
  (:use [clojure.test])
  (:use [effectivejava.test.helper])
  (:require [instaparse.core :as insta]))

(def javaparserCus (cus "test-resources/sample-codebases/javaparser/"))

(deftest testParseFileByNameOnAccessSpecifier
  (let [clazzes (flatten (map allClasses javaparserCus))]
    (is (= 
         #{"japa.parser.JavaParser" "japa.parser.ast.body.ModifierSet" "japa.parser.ASTHelper" "japa.parser.SourcesHelper" "japa.parser.PositionUtils"}
         (set (map getQName (filter isUtilClass? clazzes)))))))

