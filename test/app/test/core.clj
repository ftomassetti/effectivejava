(ns app.test.core
  (:use [app.core])
  (:use [app.test.helper])
  (:use [app.itemsOnLifecycle])
  (:use [app.interactive])
  (:use [clojure.test])
  (:use [conjure.core])
  (:require [instaparse.core :as insta]))

(load "helper")

; ============================================
; usageError
; ============================================

(deftest testWithAMessageWithoutErrors
  (mocking [println]
    (usageError "myBanner" {:errors nil} "bad, bad mistake my friend!")
    (verify-call-times-for println 2)
    (verify-first-call-args-for println "Incorrect usage: bad, bad mistake my friend!"))
    ; TODO verify also the second call to println, if conjure supports it
  )

(deftest testWithoutAMessageWithoutErrors
  (mocking [println]
    (usageError "myBanner" {:errors nil} nil)
    (verify-call-times-for println 2)
    (verify-first-call-args-for println "Incorrect usage"))
  ; TODO verify also the second call to println, if conjure supports it
  )

(deftest testWithoutAMessageWithErrors
  (mocking [println]
    (usageError "myBanner" {:errors ["bad1", "bad2"]} nil)
    (verify-call-times-for println 4)
    (verify-first-call-args-for println "Incorrect usage"))
  ; TODO verify also the other calls to println, if conjure supports it
  )

; ============================================
; info
; ============================================

(deftest testInfo
  (mocking [println]
    (info "foo")
    (verify-call-times-for println 1)
    (verify-first-call-args-for println " [info] " "foo")))

; ============================================
; ConflictingOptions
; ============================================

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

; ============================================
; treat-possible-errors
; ============================================

(deftest treat-possible-errors-when-everything-is-fine
  (mocking [usageError exit-error!]
    (treat-possible-errors {:errors nil} "my nice banner")
    (verify-call-times-for usageError 0)
    (verify-call-times-for exit-error! 0)))

(deftest treat-possible-errors-when-errors-are-passed
  (mocking [usageError exit-error!]
    (treat-possible-errors {:errors ["bad1", "bad2"]} "my nice banner")
    (verify-call-times-for usageError 1)
    (verify-first-call-args-for usageError "my nice banner" {:errors ["bad1", "bad2"]} "")
    (verify-call-times-for exit-error! 1)
    (verify-first-call-args-for exit-error!)))

(deftest treat-possible-errors-with-conflicting-options
  (mocking [usageError exit-error!]
    (treat-possible-errors {:linter true, :interactive true} "my nice banner")
    (verify-call-times-for usageError 1)
    (verify-first-call-args-for usageError "my nice banner"{:linter true, :interactive true} self-exclusive-modes-error)
    (verify-call-times-for exit-error! 1)
    (verify-first-call-args-for exit-error!)))

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

(deftest testClassWithCommentedCallToFinalize
  (let [cl (parseType "ClassWithCommentedCallToFinalize")]
    (is (false? (calls-finalizers? cl)))))

(deftest testClassWithCallToFinalizeWithParams
  (let [cl (parseType "ClassWithCallToFinalizeWithParams")]
    (is (false? (calls-finalizers? cl)))))

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
