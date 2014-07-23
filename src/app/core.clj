(ns app.core
  (:require [clojure.java.io :as io])
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:require [instaparse.core :as insta])
  (:gen-class :main true))

; ============================================
; Misc
; ============================================

(def not-nil? (complement nil?))

; ============================================
; Files
; ============================================

(defn java-file? [f]
  (and (.isFile f)
    (.endsWith (.getName f) ".java")))

(defn java-files [dirname]
  (let [d (java.io.File. dirname)]
    (filter java-file? (file-seq d))))

(load "javaparser")

(load "itemsOnLifecycle")

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

(defn -main
  "What I do"
  [& args]
  (let [optsMap
    (parse-opts args
      [
        ["-h" "--help" "Show help" :flag true :default false]
        ["-i" "--interactive" "launch interactive move" :flag true :default false]
        ["-d" "--dir DIRNAME" "REQUIRED: Directory containing the code to check"]
        ["-q" "--query QUERYNAME" "REQUIRED: Query to perform: mc=many constructors, mcp=many constructor parameters, st=singleton type"]
        ["-t" "--threshold VALUE" "Threshold to be used in the query" :default 0
         :parse-fn #(Integer/parseInt %)
         :validate [#(>= % 0) "Must be a number equal or greater to 0"]]
      ])
    opts (:options optsMap)
    banner (:summary optsMap)]
    (do
      (when (:interactive opts)
        (do
          (interactive {})
          (System/exit 0)))
      (when (:help opts)
        (do
          (println ("Printing help message, as asked"))
          (println banner))
        (System/exit 0))
      (if
        (and
          (:dir opts)
          (:query opts)
          (name2query (:query opts))
          (nil? (:errors opts)))
        (run opts)
        (do
          (println "Incorrect usage")
          (when (:errors opts)
            (doseq [e (:errors opts)]
              (println " * " e)))
          (println banner)
          (System/exit 1))))))
