(ns app.test.javaparser
  (:use [app.core])
  (:use [app.itemsOnLifecycle])
  (:use [app.model.protocols])
  (:use [app.model.javaparser])
  (:use [clojure.test])
  (:use [app.test.helper])
  (:require [instaparse.core :as insta]))

(load "helper")

; ============================================
; Parsing
; ============================================

(deftest testParsingFileWithErrorsReturnNil
  (is (nil? (parseResource "ClassWithErrors"))))

; ============================================
; Recognize node types
; ============================================

(deftest testIsClassPositive
  (is (isClass? (parseType "ASimpleClass"))))

(deftest testIsClassNegative_Interface
  (is (not (isClass? (parseType "ASimpleInterface")))))

(deftest testIsClassNegative_Enum
  (is (not (isClass? (parseType "ASimpleEnum")))))

(deftest testIsInterfacePositive
  (is (isInterface? (parseType "ASimpleInterface"))))

(deftest testIsInterfaceNegative_Class
  (is (not (isInterface? (parseType "ASimpleClass")))))

(deftest testIsInterfaceNegative_Enum
  (is (not (isInterface? (parseType "ASimpleEnum")))))

(deftest testIsEnumPositive
  (is (isEnum? (parseType "ASimpleEnum"))))

(deftest testIsEnumNegative_Class
  (is (not (isEnum? (parseType "ASimpleClass")))))

(deftest testIsEnumNegative_Interface
  (is (not (isEnum? (parseType "ASimpleInterface")))))

; ============================================
; Modifiers
; ============================================

(deftest testGetModifiersOnAType_NoModifiers
  (let [t (parseType "ASimpleClass")]
    (is (zero? (getModifiers t)))))

(deftest testGetModifiersOnAType_TwoModifiers
  (let [t (parseType "ASimplePublicFinalClass")]
    (is (= 17 (getModifiers t)))))

(deftest hasPackageLevelAccess?_Positive
  (let [t (parseType "ASimpleClass")]
    (is (hasPackageLevelAccess? t))))

(deftest hasPackageLevelAccess?_Negative
  (let [t (parseType "ASimplePublicFinalClass")]
    (is (not (hasPackageLevelAccess? t)))))

; ============================================
; Naming
; ============================================

(deftest packageNameCompilationUnitEmpty
  (let [cu (parseResource "ASimpleClass")]
    (is (= "" (packageName cu)))))

(deftest packageNameCompilationUnitNotEmpty
  (let [cu (parseResource "ASimplePackage")]
    (is (= "a.simple.package_" (packageName cu)))))

(deftest packageNameNodeEmpty
  (let [t (parseType "ASimpleClass")]
    (is (= "" (packageName t)))))

(deftest packageNameNodeNotEmpty
  (let [t (parseType "ASimpleClassInAPackage")]
    (is (= "some.path" (packageName t)))))

(deftest packageNameSingleFieldDeclarationEmpty
  (let [sfd (parseTypeMember "ASimpleClass")]
    (is (= "" (packageName sfd)))))

(deftest packageNameSingleFieldDeclarationNotEmpty
  (let [sfd (parseTypeMember "ASimpleClassInAPackage")]
    (is (= "some.path" (packageName sfd)))))

; ============================================
; Accessing nodes
; ============================================

(deftest testTopLevelTypes
  (let [cu (parseResource "AnnidatedTypes")
        tlt (topLevelTypes cu)
        tltNames (set (map getQName tlt))]
    (is (= #{"E1" "I3"} tltNames))))

(deftest testAllTypes
  (let [cu (parseResource "AnnidatedTypes")
        at (allTypes cu)
        atNames (set (map getQName at))]
    (is (= #{"E1" "E1_C1" "E1_C1_I1" "E1_C1_C2" "E1_I2" "I3"} atNames))))
