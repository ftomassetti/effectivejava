(ns app.test.helper
  (:use [app.model.javaparser])
  (:use [clojure.test])
  (:use [app.javaparser.parsing]))

(defn readResource [filename]
  (let [resourceName (str "app/test/samples/" filename ".java.txt")
        code (slurp (clojure.java.io/resource resourceName))]
    code))

(defn parseResource [filename]
  (parseString (readResource filename)))

(defn parseType [filename]
  (let [cu (parseResource filename)
        cl (first (.getTypes cu))]
    cl))

(defn parseTypeMember [filename]
  (first (.getMembers (parseType filename))))