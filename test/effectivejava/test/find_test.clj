(ns effectivejava.test.find_test
  (:use [effectivejava.core])
  (:use [effectivejava.test.helper])
  (:use [effectivejava.find])
  (:use [effectivejava.model.protocols])
  (:use [effectivejava.model.javaparser])
  (:use [effectivejava.javaparser.facade])
  (:use [effectivejava.symbol_solver.type_solver])
  (:use [effectivejava.symbol_solver.scope])
  (:use [clojure.test])
  (:use [conjure.core])
  (:require [instaparse.core :as insta]))

(def javaparser2 "test-resources/sample-codebases/javaparser")
(def javaparser-cus (cus javaparser2))

; ============================================
; type-exact-match?
; ============================================

(deftest test-type-exact-match
  (let [cu (parseResource "TypesToMatch")
        c (first (allTypes cu))
        type-a (.getType (.get (.getMembers c) 0))
        type-array-a (.getType (.get (.getMembers c) 1))
        type-qname-a (.getType (.get (.getMembers c) 2))
        type-array-qname-a (.getType (.get (.getMembers c) 3))]
    (binding [typeSolver (jreTypeSolver)]
      ; first match types with themselves
      (is (type-exact-match? type-a type-a))
      (is (type-exact-match? type-qname-a type-qname-a))
      (is (type-exact-match? type-array-a type-array-a))
      (is (type-exact-match? type-array-qname-a type-array-qname-a))
      ; then match the equivalent
      (is (type-exact-match? type-a type-qname-a))
      (is (type-exact-match? type-array-a type-array-qname-a))
      ; then verify the others does not match
      (is (not (type-exact-match? type-a type-array-a))))))

(deftest test-type-exact-match-on-equals
  (binding [typeSolver (jreTypeSolver)]
    (let [m1 (first (find-methods-by-qname javaparser-cus "japa.parser.ast.Node.equals"))
          m1-p (first (.getParameters m1))
          expected-type (make-reference-type-ref "java.lang.Object" nil)
          paramType (.getType m1-p)]
      (is (type-exact-match? paramType expected-type)))))

; ============================================
; param-match-type?
; ============================================

(deftest test-param-match-type-on-equals
  (binding [typeSolver (jreTypeSolver)]
    (let [m1 (first (find-methods-by-qname javaparser-cus "japa.parser.ast.Node.equals"))
          m1-p (first (.getParameters m1))
          expected-type (make-reference-type-ref "java.lang.Object" nil)
          pair [m1-p expected-type]]
      (is (param-match-type? pair)))))

; ============================================
; find-methods-by-name
; ============================================

(deftest test-find-methods-by-name
  (let [res (find-methods-by-name javaparser-cus "equals")]
    (is (= 2 (count res)))))

; ============================================
; method-match-exactly-parameters?
; ============================================

(deftest test-method-match-exactly-parameters-on-equals
  (binding [typeSolver (jreTypeSolver)]
    (let [m1 (first (find-methods-by-qname javaparser-cus "japa.parser.ast.Node.equals"))
          exp-types [(make-reference-type-ref "java.lang.Object" nil)]]
      (is (method-match-exactly-parameters? exp-types m1)))))

; ============================================
; find-methods-by-signature
; ============================================

; should find one because the other method named equals has this signature:
; public static boolean equals(final Node n1, final Node n2)
(deftest test-find-methods-by-signature-on-equals
  (binding [typeSolver (jreTypeSolver)]
    (let [res (find-methods-by-signature javaparser-cus "equals" [(make-reference-type-ref "java.lang.Object" nil)])]
      (is (= 1 (count res))))))
