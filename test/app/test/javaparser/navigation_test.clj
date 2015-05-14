(ns app.test.javaparser.navigation_test
  (:use [app.itemsOnLifecycle])
  (:use [app.model.protocols])
  (:use [app.model.javaparser])
  (:use [app.javaparser.navigation])
  (:use [app.test.helper])
  (:use [clojure.test]))

; ============================================
; topLevelTypes
; ============================================

(deftest testTopLevelTypes
  (let [cu (parseResource "AnnidatedTypes")
        tlt (topLevelTypes cu)
        tltNames (set (map getQName tlt))]
    (is (= #{"E1" "I3"} tltNames))))

; ============================================
; allTypes
; ============================================

(deftest test-all-types1
  (let [cu (parseResource "AnnidatedTypes")
        at (allTypes cu)
        atNames (set (map getQName at))]
    (is (= #{"E1" "E1_C1" "E1_C1_I1" "E1_C1_C2" "E1_I2" "I3"} atNames))))

(deftest test-all-types2
  (let [cu (parseResource "LotOfTypes")
        at (allTypes cu)
        atNames (set (map getQName at))]
    (is (= #{"TopClass" "Intf1" "Enum1" "Intf2" "Class2" "Class3" "Class4" "Enum2"} atNames))))

; ============================================
; allClasses
; ============================================

(deftest test-all-classes
  (let [cu (parseResource "LotOfTypes")
        at (allClasses cu)
        atNames (set (map getQName at))]
    (is (= #{"TopClass" "Class2" "Class3" "Class4"} atNames))))

; ============================================
; allInterfaces
; ============================================

(deftest test-all-interfaces
  (let [cu (parseResource "LotOfTypes")
        at (allInterfaces cu)
        atNames (set (map getQName at))]
    (is (= #{"Intf1" "Intf2"} atNames))))

; ============================================
; allEnums
; ============================================

(deftest test-all-enums
  (let [cu (parseResource "LotOfTypes")
        at (allEnums cu)
        atNames (set (map getQName at))]
    (is (= #{"Enum1" "Enum2"} atNames))))