(ns app.test.core
  (:use [app.core])
  (:use [clojure.test]))

(defn parseClass [filename]
  (let [resourceName (str "app/test/samples/" filename ".java.txt")
        code (slurp (clojure.java.io/resource resourceName))
        cu (parseString code)
        cl (first (getClasses cu))]
    cl))

(deftest isPublicFieldSingletonPositive
  (let [cl (parseClass "ClassWithPublicFieldSingleton")]
    (is (isPublicFieldSingleton? cl))))

(deftest isPublicFieldSingletonNotNamedInstance
  (let [cl (parseClass "ClassWithoutPublicFieldSingleton_NotNamedInstance")]
    (is (not (isPublicFieldSingleton? cl)))))

(deftest isPublicFieldSingletonNotPublic
  (let [cl (parseClass "ClassWithoutPublicFieldSingleton_NotPublic")]
    (is (not (isPublicFieldSingleton? cl)))))

(deftest isPublicFieldSingletonNotStatic
  (let [cl (parseClass "ClassWithoutPublicFieldSingleton_NotStatic")]
    (is (not (isPublicFieldSingleton? cl)))))
