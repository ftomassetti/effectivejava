; ============================================
; CLI
; ============================================

(defn name2operation [name]
  (cond
    (= "mc" name) classesWithManyConstructorsOp
    (= "mcp" name) constructorsWithManyParametersOp
    (= "st" name) classesAndSingletonTypeOp
    :else nil))

(defn run [opts]
  (let [dirname (:dir opts)
        th (:threshold opts)
        operation (name2operation (:query opts))
        cus (filter not-nil? (cus dirname))]
    (do
      (println "Considering" (.size cus) "Java files")
      (printOperation operation cus th))))
