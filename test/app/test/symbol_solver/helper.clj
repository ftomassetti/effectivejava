(ns app.test.symbol_solver.helper
  (:use [app.jarloading])
  (:use [app.model.protocols])
  (:use [app.model.javaparser])
  (:use [app.javaparser.navigation])
  (:use [app.symbol_solver.funcs])
  (:use [app.symbol_solver.type_solver])
  (:use [app.symbol_solver.scope])
  (:use [app.utils])
  (:use [clojure.test]))

(def javaparser2 "test-resources/sample-jars/javaparser-core-2.0.0.jar")
(def samplesCus (cus "test-resources/sample-codebases/samples/"))
(def sampleClasses (flatten (map allTypes samplesCus)))

(defn sampleClass [name]
  {:post [%]}
  (first (filter (fn [c] (= name (.getName c))) sampleClasses)))
