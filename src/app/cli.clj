; ============================================
; CLI
; ============================================

(def cliOpts [
               ["-h" "--help" "Show help" :flag true :default false]
               ["-i" "--interactive" "launch interactive mode" :flag true :default false]
               ["-l" "--linter" "launch linter mode" :flag true :default false]
               ["-d" "--dir DIRNAME" "REQUIRED: Directory containing the code to check"]
               ["-q" "--query QUERYNAME" "REQUIRED: Query to perform: mc=many constructors, mcp=many constructor parameters, st=singleton type"]
               ["-t" "--threshold VALUE" "Threshold to be used in the query" :default 0
                :parse-fn #(Integer/parseInt %)
                :validate [#(>= % 0) "Must be a number equal or greater to 0"]]
               ])

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
      (println "Considering" (.size cus) "Java files")
      (printOperation operation cus th)))
