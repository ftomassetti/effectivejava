(ns app.test.core
  (:use [app.core])
  (:use [clojure.test])
  (:require [instaparse.core :as insta]))

(defn readResource [filename]
  (let [resourceName (str "app/test/samples/" filename ".java.txt")
        code (slurp (clojure.java.io/resource resourceName))]
    code))

(defn parseResource [filename]
  (parseString (readResource filename)))

; TODO remove this method and use parseType
(defn parseClass [filename]
  (let [cu (parseResource filename)
        cl (first (getClasses cu))]
    cl))

(defn parseType [filename]
  (let [cu (parseResource filename)
        cl (first (.getTypes cu))]
    cl))

; ============================================
; Parsing
; ============================================

(deftest testParsingFileWithErrorsReturnNil
  (is (= nil (parseResource "ClassWithErrors"))))

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
    (is (= 0 (getModifiers t)))))

(deftest testGetModifiersOnAType_TwoModifiers
  (let [t (parseType "ASimplePublicFinalClass")]
    (is (= 17 (getModifiers t)))))

; ============================================
; Other FIXME organize!
; ============================================

(deftest testIsPublicFieldSingletonPositive
  (let [cl (parseClass "ClassWithPublicFieldSingleton")]
    (is (isPublicFieldSingleton? cl))))

(deftest testIsPublicFieldSingletonNotNamedInstance
  (let [cl (parseClass "ClassWithoutPublicFieldSingleton_NotNamedInstance")]
    (is (not (isPublicFieldSingleton? cl)))))

(deftest testIsPublicFieldSingletonNotPublic
  (let [cl (parseClass "ClassWithoutPublicFieldSingleton_NotPublic")]
    (is (not (isPublicFieldSingleton? cl)))))

(deftest testIsPublicFieldSingletonNotStatic
  (let [cl (parseClass "ClassWithoutPublicFieldSingleton_NotStatic")]
    (is (not (isPublicFieldSingleton? cl)))))

(deftest testIsPublicMethodSingletonPositive
  (let [cl (parseClass "ClassWithPublicMethodSingleton")]
    (is (isPublicMethodSingleton? cl))))

(deftest testIsPublicMethodSingletonNotNamedGetInstance
  (let [cl (parseClass "ClassWithoutPublicMethodSingleton_NotNamedGetInstance")]
    (is (not (isPublicMethodSingleton? cl)))))

(deftest testIsSingletonEnum?
  (let [cl (parseType "SingletonEnum")]
    (is (isSingletonEnum? cl))))

(deftest testIsNotSingletonEnumNoInstance
  (let [cl (parseType "NotSingletonEnum_NoInstance")]
    (is (not (isSingletonEnum? cl)))))

(deftest testIsNotSingletonEnumNotOnlyInstance
  (let [cl (parseType "NotSingletonEnum_NotOnlyInstance")]
    (is (not (isSingletonEnum? cl)))))

; =============================================================
; Command parser
; =============================================================

(deftest testUnknown
  (is (insta/failure? (command-parser "a not valid command"))))

(deftest testParsingQ
  (is (= [:EXIT 'q']) (command-parser "q")))

(deftest testParsingQuit
  (is (= [:EXIT 'quit']) (command-parser "quit")))

(deftest testParsingExit
  (is (= [:EXIT 'exit']) (command-parser "exit")))