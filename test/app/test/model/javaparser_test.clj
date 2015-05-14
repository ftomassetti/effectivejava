(ns app.test.model.javaparser_test
  (:use [app.itemsOnLifecycle])
  (:use [app.model.protocols])
  (:use [app.model.javaparser])
  (:use [app.javaparser.navigation])
  (:use [app.test.helper])
  (:use [clojure.test]))

; ============================================
; getQName for TypeDeclaration
; ============================================

(deftest test-getQName-for-TypeDeclaration
  (let [cu (parseResource "AnnidatedTypes")
        e1 (first (topLevelTypes cu))
        _ (assert e1)
        e1-c1 (.get (.getMembers e1) 0)
        _ (assert e1-c1)
        e1-i2 (.get (.getMembers e1) 1)
        _ (assert e1-i2)
        e1-c1-i1 (.get (.getMembers e1-c1) 0)
        _ (assert e1-c1-i1)
        e1-c1-c2 (.get (.getMembers e1-c1) 1)
        _ (assert e1-c1-c2)]
    (is (= "E1" (getQName e1)))
    (is (= "E1.E1_C1" (getQName e1-c1)))
    (is (= "E1.E1_I2" (getQName e1-i2)))
    (is (= "E1.E1_C1.E1_C1_I1" (getQName e1-c1-i1)))
    (is (= "E1.E1_C1.E1_C1_C2" (getQName e1-c1-c2)))))

