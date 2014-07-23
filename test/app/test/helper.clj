(defn readResource [filename]
  (let [resourceName (str "app/test/samples/" filename ".java.txt")
        code (slurp (clojure.java.io/resource resourceName))]
    code))

(defn parseResource [filename]
  (parseString (readResource filename)))

; TODO remove this method and use parseType
(defn parseClass [filename]
  (let [cu (parseResource filename)
        cl (first (getClasses cu))]
    cl))

(defn parseType [filename]
  (let [cu (parseResource filename)
        cl (first (.getTypes cu))]
    cl))
