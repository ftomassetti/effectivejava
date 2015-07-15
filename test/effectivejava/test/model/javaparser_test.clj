(ns effectivejava.test.model.javaparser_test
  (:use [effectivejava.itemsOnLifecycle])
  (:use [effectivejava.model.protocols])
  (:use [effectivejava.model.javaparser])
  (:use [effectivejava.javaparser.facade])
  (:use [effectivejava.test.helper])
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

; ============================================
; TypeRef
; ============================================

(deftest test-RefType-array-primitive
  (let [cu (parseResource "RefTypes")
        c (first (topLevelTypes cu))
        foo1 (.getType (.get (.getMembers c) 0))
        foo1-base (baseType foo1)
        foo2 (.getType (.get (.getMembers c) 1))
        foo2-base (baseType foo2)
        foo2-base-base (baseType foo2-base)
        foo2-base-base-base (baseType foo2-base-base)]
    (is (array? foo1))
    (is (not (primitive? foo1)))
    (is (not (reference-type? foo1)))

    (is (not (array? foo1-base)))
    (is (primitive? foo1-base))
    (is (not (reference-type? foo1-base)))
    (is (= "int" (typeName foo1-base)))

    (is (array? foo2))
    (is (not (primitive? foo2)))
    (is (not (reference-type? foo2)))

    (is (array? foo2-base))
    (is (not (primitive? foo2-base)))
    (is (not (reference-type? foo2-base)))

    (is (array? foo2-base-base))
    (is (not (primitive? foo2-base-base)))
    (is (not (reference-type? foo2-base-base)))

    (is (not (array? foo2-base-base-base)))
    (is (primitive? foo2-base-base-base))
    (is (not (reference-type? foo2-base-base-base)))
    (is (= "int" (typeName foo2-base-base-base)))))

(deftest test-RefType-array-reference
  (let [cu (parseResource "RefTypes")
        c (first (topLevelTypes cu))
        foo3 (.getType (.get (.getMembers c) 2))
        foo3-base (baseType foo3)
        foo4 (.getType (.get (.getMembers c) 3))
        foo4-base (baseType foo4)
        foo4-base-base (baseType foo4-base)
        foo4-base-base-base (baseType foo4-base-base)]
    (is (array? foo3))
    (is (not (primitive? foo3)))
    (is (not (reference-type? foo3)))

    (is (not (array? foo3-base)))
    (is (not (primitive? foo3-base)))
    (is (reference-type? foo3-base))
    (is (= "A" (typeName foo3-base)))

    (is (array? foo4))
    (is (not (primitive? foo4)))
    (is (not (reference-type? foo4)))

    (is (array? foo4-base))
    (is (not (primitive? foo4-base)))
    (is (not (reference-type? foo4-base)))

    (is (array? foo4-base-base))
    (is (not (primitive? foo4-base-base)))
    (is (not (reference-type? foo4-base-base)))

    (is (not (array? foo4-base-base-base)))
    (is (not (primitive? foo4-base-base-base)))
    (is (reference-type? foo4-base-base-base))
    (is (= "A" (typeName foo4-base-base-base)))))