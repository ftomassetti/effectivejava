(ns app.test.jarloading
  (:use [app.jarloading])
  (:use [clojure.test]))

(def javaparser2 "test-resources/sample-jars/javaparser-core-2.0.0.jar")

(deftest testGetElementsEntriesInJar
  (is (= 137 (count (getElementsEntriesInJar javaparser2)))))

(deftest testGetClassesEntriesInJar
  (is (= 134 (count (getClassesEntriesInJar javaparser2)))))


