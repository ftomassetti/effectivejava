(ns app.test.core
  (:use [app.core])
  (:use [clojure.test]))

(defn parseClass [filename]
  (let [resourceName (str "app/test/samples/" filename ".java.txt")
        code (slurp (clojure.java.io/resource resourceName))
        cu (parseString code)
        cl (first (getClasses cu))]
    cl))

(defn parseType [filename]
  (let [resourceName (str "app/test/samples/" filename ".java.txt")
        code (slurp (clojure.java.io/resource resourceName))
        cu (parseString code)
        cl (first (.getTypes cu))]
    cl))

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