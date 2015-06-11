(ns app.test.core
  (:use [app.core])
  (:use [app.test.helper])
  (:use [app.itemsOnLifecycle])
  (:use [app.interactive])
  (:use [app.javaparser.navigation])
  (:use [clojure.test])
  (:use [conjure.core])
  (:use [app.symbol_solver.funcs])
  (:use [app.symbol_solver.type_solver])
  (:require [instaparse.core :as insta]))

(load "helper")

(def sampleClassesItem10Test (cus "test-resources/sample-codebases/samples/test_item10"))

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
           (verify-first-call-args-for usageError "my nice banner" {:linter true, :interactive true} self-exclusive-modes-error)
           (verify-call-times-for exit-error! 1)
           (verify-first-call-args-for exit-error!)))

; ============================================
; Item 3 - Singletons
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

; ============================================
; Item 7 - Avoid finalizers
; ============================================

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

; ============================================
; Item 10 - Override toString()
; ============================================

(deftest testClassThatOverridesToString
  (let [type-solver-classes (flatten (map allTypes sampleClassesItem10Test))
        cl (first (filter #(= (.getName %) "ClassThatOverridesToString")
                          type-solver-classes))]
    (is (false? (does-not-override-toString-but-should?
                 type-solver-classes cl)))))

(deftest testClassWhoseParentsOverrideToString
  (let [type-solver-classes (flatten (map allTypes sampleClassesItem10Test))
        cl (first (filter #(= (.getName %) "ClassWhoseParentOverridesToString")
                          type-solver-classes))]
    (is (false? (does-not-override-toString-but-should?
                 type-solver-classes cl)))))

(deftest testClassThatDeclaresToStringWithParams
  (let [type-solver-classes (flatten (map allTypes sampleClassesItem10Test))
        cl (first (filter
                   #(= (.getName %) "ClassThatDeclaresToStringWithParams")
                   type-solver-classes))]
    (is (does-not-override-toString-but-should? type-solver-classes cl))))

(deftest testClassThatDoesNotOverrideToString
  (let [type-solver-classes (flatten (map allTypes sampleClassesItem10Test))
        cl (first (filter #(= (.getName %) "ClassThatDoesNotOverrideToString")
                          type-solver-classes))]
    (is (does-not-override-toString-but-should? type-solver-classes cl))))

(deftest testUtilClassThatDoesNotOverrideToString
  (let [type-solver-classes (flatten (map allTypes sampleClassesItem10Test))
        cl (first (filter #(= (.getName %) "UtilsClassDoesNotOverrideToString")
                          type-solver-classes))]
    (is (false? (does-not-override-toString-but-should?
                 type-solver-classes cl)))))

(deftest abstractClassDoesNotNeedToOverrideToString
  (let [type-solver-classes (flatten (map allTypes sampleClassesItem10Test))
        cl (first (filter #(= (.getName %) "AbstractClassDoesNotOverrideToString")
                          type-solver-classes))]
    (is (false? (does-not-override-toString-but-should?
                 type-solver-classes cl)))))

; ============================================
; Type solver gets superclasses correctly
; ============================================

(deftest test-getAllSuperclasses-depth-0
  (let [type-solver-classes (flatten (map allTypes sampleClassesItem10Test))
        cl (first (filter #(= (.getName %) "ParentClassThatOverridesToString")
                          type-solver-classes))]
    (binding [typeSolver (typeSolverOnList type-solver-classes)]
      (let [superclasses (getAllSuperclasses cl)]
        (is (= 0 (count superclasses)))))))

(deftest test-getAllSuperclasses-depth-1
  (let [type-solver-classes (flatten (map allTypes sampleClassesItem10Test))
        cl (first (filter #(= (.getName %) "ClassWhoseParentOverridesToString")
                          type-solver-classes))]
    (binding [typeSolver (typeSolverOnList type-solver-classes)]
      (let [superclasses (getAllSuperclasses cl)]
        (is (= 1 (count superclasses)))))))

; ============================================
; Command parser
; ============================================

(deftest testUnknown
  (is (insta/failure? (command-parser "a not valid command"))))

(deftest testParsingQ
  (is (= '([:EXIT "q"]) (command-parser "q"))))

(deftest testParsingQuit
  (is (= '([:EXIT "quit"]) (command-parser "quit"))))

(deftest testParsingExit
  (is (= '([:EXIT "exit"]) (command-parser "exit"))))
