(ns app.test.operations
  (:use [app.core])
  (:use [app.operations])
  (:use [app.itemsOnLifecycle])
  (:use [app.model.javaparser])
  (:use [clojure.test])
  (:use [app.test.helper])
  (:require [instaparse.core :as insta]))

(deftest testFormatValueNoCutOnStartNeeded
  (is (= "short     " (formatValue "short" 10 :onStart))))

(deftest testFormatValueCutOnStartNeeded
  (is (= "ingToBeCut" (formatValue "aLongStringToBeCut" 10 :onStart))))

(deftest testFormatValueNoCuOnEndtNeeded
  (is (= "short     " (formatValue "short" 10 :onEnd))))

(deftest testFormatValueCutOnEndNeeded
  (is (= "aLongStrin" (formatValue "aLongStringToBeCut" 10 :onEnd))))

(deftest testFieldValueToStr
  (is (= "aFantastic | " (fieldValueToStr {:len 10} "aFantasticValue"))))

(deftest testFieldValuesToStrWithOneField
  (is (= "aFantastic | " (fieldValuesToStr [{:len 10}] ["aFantasticValue"]))))

(deftest testFieldValuesToStrWithTwoFields
  (is (= "aFantastic | 10 | " (fieldValuesToStr [{:len 10} {:len 2}] ["aFantasticValue", 10]))))

(deftest testRowStr
  (is (= "ciao    | 1  " (rowStr [7 3] ["ciao" "1"]))))

(deftest testColumnLengthRowIsLongerThanHeader
  (is (= 5 (columnLength "ciao" [["a" "zzzzz" "bc"]] 1))))

(deftest testColumnLengthHeaderIsTheLongest
  (is (= 9 (columnLength "ciaobello" [["a" "zzzzz" "bc"]] 1))))

(deftest testToStringClassDeclaration
  (let [t (parseType "ASimpleClass")]
    (is (= "ASimpleClass" (toString t)))))

(deftest testToStringInteger
  (is (= "123" (toString 123))))

(deftest testResultsToStrings
  (is (= '(["row1" "1"] ["row2" "2"]) (resultsToStrings '(["row1" 1] ["row2" 2])))))
