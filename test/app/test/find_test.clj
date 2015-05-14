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

(def javaparser2 "test-resources/sample-codebases/javaparser")
(def javaparser-cus (cus javaparser2))

; ============================================
; type-exact-match?
; ============================================

(def test-type-exact-match
  (let [cu (parseResource "TypesToMatch")
        c (first (allTypes cu))
        type-a (.getType (.get (.getMembers c) 0))
        type-array-a (.getType (.get (.getMembers c) 1))
        type-qname-a (.getType (.get (.getMembers c) 2))
        type-array-qname-a (.getType (.get (.getMembers c) 3))]
    (binding [typeSolver (jreTypeSolver)]
      (is (type-exact-match? type-a type-qname-a))
      (is (type-exact-match? type-array-a type-array-qname-a))
      (is (not (type-exact-match? type-a type-array-a))))))

; ============================================
; find-methods-by-signature
; ============================================

(deftest find-methods-by-signature-on-equals
  (binding [typeSolver (jreTypeSolver)]
    (let [res (find-methods-by-signature javaparser-cus "equals" [(make-reference-type-ref "java.lang.Object" nil)])]
      (is (= 2 (count res))))))