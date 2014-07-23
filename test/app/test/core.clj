(ns app.test.core
  (:use [app.core])
  (:use [clojure.test])
  (:require [instaparse.core :as insta]))

(load "helper")
(load "javaparser")

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