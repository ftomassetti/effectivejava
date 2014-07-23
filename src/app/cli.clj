; ============================================
; CLI
; ============================================

(defn name2query [name]
  (cond
    (= "mc" name) printClassesWithManyConstructors
    (= "mcp" name) printConstructorsWithManyParameters
    (= "st" name) printSingletonType
    :else nil))

(defn run [opts]
  (let [dirname (:dir opts)
        th (:threshold opts)
        query (name2query (:query opts))
        cus (filter not-nil? (cus dirname))]
    (do
      (println "Considering" (.size cus) "Java files")
      (query cus th))))
