(ns app.test.find_test
  (:use [app.core])
  (:use [app.test.helper])
  (:use [app.find])
  (:use [app.model.protocols])
  (:use [app.javaparser.parsing])
  (:use [app.javaparser.navigation])
  (:use [app.symbol_solver.type_solver])
  (:use [clojure.test])
  (:use [conjure.core])
  (:require [instaparse.core :as insta]))

; ============================================
; find-methods-by-signature
; ============================================

(def javaparser2 "test-resources/sample-codebases/javaparser")
(def javaparser-cus (cus javaparser2))

(deftest find-methods-by-signature-on-equals
  (binding [typeSolver (jreTypeSolver)]
    (let [res (find-methods-by-signature javaparser-cus "equals" [(make-reference-type-ref "java.lang.Object" nil)])]
      (is (= 2 (count res))))))