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
      (let [command (first (first ast))]
        (cond
          (= command :EXIT) (println "Exit...")
          (= command :LIST)
          (do
            (let [loadedCus (:cus state)]
              (if loadedCus
                (do
                  (println "Listing types currently loaded:")
                  (doseq [cu (:cus state)]
                    (doseq [t (.getTypes cu)]
                      (println " *" (typeQname t)))))
                (println "No classes loaded. Use <load> first"))
              (rp state)))
          (= command :HELP)
          (do
            (println "Help...")
            (rp state))
          (= command :LOAD)
          (let [dirnameWithApex (nth (nth (first ast) 2) 1),
                dirname (subs dirnameWithApex 1 (+ (.length dirnameWithApex) -1))]
            (do
              (println "Loading" dirname)
              (let [loadedCus (cus dirname)]
                (println "Java files loaded:" (.size loadedCus))
                (rp {:cus loadedCus}))))
          :else (println "Command not implemented: " command))))))

(defn interactive [state]
  (do
    (print "> ")
    (flush)
    (process state (read-line) interactive)))
