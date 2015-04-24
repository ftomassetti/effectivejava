; ============================================
; Interactive mode
; ============================================

(def command-parser
  (insta/parser
    (str
      "<COMMAND> = HELP | EXIT | LOAD | LIST | MC            \n"
      "HELP = 'help' | 'h'                                   \n"
      "EXIT = 'exit' | 'quit' | 'q'                          \n"
      "LOAD = 'load' <WS> STR                                \n"
      "LIST = 'list'                                         \n"
      "MC  = ('mc'|'many-constructors') <WS> 'th' <WS> NUM   \n"
      "WS  = #'[\t ]+'                                       \n"
      "NUM = #'[0-9]+'                                       \n"
      "STR = #'\"[^\"]*\"'                                   \n")))

(defn process [state input rp]
  (let
    [ast (command-parser input)]
    (if
      (insta/failure? ast)
      (do
        (println "ERROR: " ast)
        (rp state))
      (let [command (ffirst ast)]
        (cond
          (= command :EXIT) (println "Exit...")
          (= command :LIST)
          (let [loadedCus (:cus state)]
            (if loadedCus
              (do
                (println "Listing types currently loaded:")
                (doseq [cu (:cus state)]
                  (doseq [t (.getTypes cu)]
                    (println " *" (getQName t))))
                (println "No classes loaded. Use <load> first"))
              (rp state)))
          (= command :HELP)
          (do
            (println "h/help                      : print this help message")
            (println "q/quit/exit                 : close the shell")
            (println "list                        : list classes loaded")
            (println "mc/many-constructors th NUM : list classes with NUM or more constructors")
            (rp state))
          (= command :LOAD)
          (let [dirnameWithApex (nth (nth (first ast) 2) 1),
                dirname (subs dirnameWithApex 1 (+ (.length dirnameWithApex) -1))]
              (println "Loading" dirname)
              (let [loadedCus (cus dirname)]
                (println "Java files loaded:" (.size loadedCus))
                (rp {:cus loadedCus})))
          (= command :MC)
          (do
            (printOperation classesWithManyConstructorsOp (:cus state) 5)
            (rp state))
          :else (println "Command not implemented: " command))))))

(defn interactive [state]
  (do
    (print "> ")
    (flush)
    (process state (read-line) interactive)))
