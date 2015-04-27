(ns app.test.core
  (:use [app.core])
  (:use [app.test.helper])
  (:use [app.itemsOnLifecycle])
  (:use [app.interactive])
  (:use [clojure.test])
  (:require [instaparse.core :as insta]))

(load "helper")
(load "javaparser")
(load "operations")
(load "acceptance")

; ============================================
; Other FIXME organize!
; ============================================

(deftest testIsPublicFieldSingletonPositive
  (let [cl (parseType "ClassWithPublicFieldSingleton")]
    (is (isPublicFieldSingleton? cl))))

(deftest testIsPublicFieldSingletonNotNamedInstance
  (let [cl (parseType "ClassWithoutPublicFieldSingleton_NotNamedInstance")]
    (is (not (isPublicFieldSingleton? cl)))))

(deftest testIsPublicFieldSingletonNotPublic
  (let [cl (parseType "ClassWithoutPublicFieldSingleton_NotPublic")]
    (is (not (isPublicFieldSingleton? cl)))))

(deftest testIsPublicFieldSingletonNotStatic
  (let [cl (parseType "ClassWithoutPublicFieldSingleton_NotStatic")]
    (is (not (isPublicFieldSingleton? cl)))))

(deftest testIsPublicMethodSingletonPositive
  (let [cl (parseType "ClassWithPublicMethodSingleton")]
    (is (isPublicMethodSingleton? cl))))

(deftest testIsPublicMethodSingletonNotNamedGetInstance
  (let [cl (parseType "ClassWithoutPublicMethodSingleton_NotNamedGetInstance")]
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

(deftest testClassCallsFinalizer
  (let [cl (parseType "ClassWithFinalizers")]
    (is (true? (calls-finalizers? cl)))))

(deftest testClassDoesNotCallFinalizer
  (let [cl (parseType "ClassWithoutFinalizers")]
    (is (false? (calls-finalizers? cl)))))

(deftest testConflictingOptions
  (is (true? (conflicting-options?
               {:query 'mc :linter true :interactive true})))
  (is (true? (conflicting-options?
               {:linter true :interactive true})))
  (is (true? (conflicting-options?
               {:query 'mc :linter true})))
  (is (true? (conflicting-options?
               {:query 'mc :interactive true})))
  (is (false? (conflicting-options?
                {:linter false :interactive true})))
  (is (false? (conflicting-options? {:query 'mc})))
  (is (false? (conflicting-options? {:linter true})))
  (is (false? (conflicting-options? {:interactive true}))))

; =============================================================
; Command parser
; =============================================================

(deftest testUnknown
  (is (insta/failure? (command-parser "a not valid command"))))

(deftest testParsingQ
  (is (= '([:EXIT "q"]) (command-parser "q"))))

(deftest testParsingQuit
  (is (= '([:EXIT "quit"]) (command-parser "quit"))))

(deftest testParsingExit
  (is (= '([:EXIT "exit"]) (command-parser "exit"))))
