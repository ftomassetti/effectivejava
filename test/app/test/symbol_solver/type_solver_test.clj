(ns app.test.symbol_solver.type_solver_test
  (:use [app.jarloading])
  (:use [app.javaparser])
  (:use [app.symbol_solver.type_solver])
  (:use [clojure.test]))

(def samplesCus (cus "test-resources/sample-codebases/type_solver_samples/"))
(def sampleTypes (flatten (map allTypes samplesCus)))

(deftest typeSolverOnListShouldNotSolveClassInDefaultPackage
  (let [typeSolver (typeSolverOnList sampleTypes)]
    (is (nil? (typeSolver "A")))))

(deftest typeSolverOnListShouldSolveClassInProperPackages
  (let [typeSolver (typeSolverOnList sampleTypes)]
    (is (typeSolver "com.foo.A"))
    (is (= "com.foo.A" (getQName (typeSolver "com.foo.A"))))
    (is (typeSolver "com.foo.bar.A"))
    (is (= "com.foo.bar.A" (getQName (typeSolver "com.foo.bar.A"))))))
