(ns effectivejava.test.symbol_solver.helper
  (:use [effectivejava.jarloading])
  (:use [effectivejava.model.protocols])
  (:use [effectivejava.model.javaparser])
  (:use [effectivejava.javaparser.navigation])
  (:use [effectivejava.symbol_solver.funcs])
  (:use [effectivejava.symbol_solver.type_solver])
  (:use [effectivejava.symbol_solver.scope])
  (:use [effectivejava.utils])
  (:use [clojure.test]))

(def javaparser2 "test-resources/sample-jars/javaparser-core-2.0.0.jar")
(def samplesCus (cus "test-resources/sample-codebases/samples/"))
(def sampleClasses (flatten (map allTypes samplesCus)))

(defn sampleClass [name]
  {:post [%]}
  (first (filter (fn [c] (= name (.getName c))) sampleClasses)))
