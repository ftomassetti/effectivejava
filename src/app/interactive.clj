(ns app.interactive
  (:use [app.javaparser])
  (:use [app.operations])
  (:use [app.itemsOnLifecycle])
  (:use [app.utils])
  (:require [instaparse.core :as insta])
  (:import [app.operations Operation]))

; ============================================
; Interactive mode
; ============================================

(def commands-grammar
  (str
   "<COMMAND> = HELP | EXIT | LOAD | LIST | MC | MCP | F | ST   \n"
   "HELP = 'help' | 'h'                                         \n"
   "EXIT = 'exit' | 'quit' | 'q'                                \n"
   "LOAD = 'load' <WS> STR                                      \n"
   "LIST = 'list'                                               \n"
   "MC  = ('mc'|'many-constructors') <WS> 'th' <WS> NUM         \n"
   "MCP = ('mcp'|'many-costructor-params') <WS> 'th' <WS> NUM   \n"
   "F = ('f'|'finalizers')                                      \n"
   "ST = ('st'|'singletons')                                    \n"
   "WS  = #'[\t ]+'                                             \n"
   "NUM = #'[0-9]+'                                             \n"
   "STR = #'\"[^\"]*\"'                                         \n"))

(def command-parser
  (insta/parser commands-grammar))

(declare interactive)

(defn- exit []
  (println "Exit..."))

(defn- list-loaded-classes [state]
  (let [loadedCus (:cus state)]
    (if loadedCus
      (do
        (println "Listing types currently loaded:")
        (doseq [cu (:cus state)]
          (doseq [t (.getTypes cu)]
            (println " *" (getQName t)))))
      (println "No classes loaded. Use <load> first"))
    (interactive state)))

(defn- help [state]
  (println "h/help                             : print this help message")
  (println "q/quit/exit                        : close the shell")
  (println "list                               : list classes loaded")
  (println "load DIR                           : load classes from DIR")
  (println "mc/many-constructors th NUM        : list classes with NUM or more constructors")
  (println "mcp/many-constructor-params th NUM : list constructors with NUM or more parameters")
  (println "f/finalizers                       : list classes that use finalizers")
  (println "st/singletons                      : list singletons")
  (interactive state))

(defn- load-classes [ast]
  (let [dirnameWithApex (nth (nth (first ast) 2) 1)
        dirname (subs dirnameWithApex 1 (dec (count dirnameWithApex)))]
    (println "Loading" dirname)
    (let [loadedCus (cus dirname)]
      (println "Java files loaded:" (count loadedCus))
      (interactive {:cus loadedCus}))))

(defn- mc-operation [state threshold]
  (printOperation classesWithManyConstructorsOp (:cus state) threshold)
  (interactive state))

(defn- mcp-operation [state threshold]
  (printOperation constructorsWithManyParametersOp (:cus state) threshold)
  (interactive state))

(defn- f-operation [state]
  (printOperation finalizersOp (:cus state) nil)
  (interactive state))

(defn- st-operation [state]
  (printOperation classesAndSingletonTypeOp (:cus state) nil)
  (interactive state))

(defn- command-not-implemented [command state]
  (println "Command not implemented: " command)
  (interactive state))

(defn- process [state input]
  (let
   [ast (command-parser input)]
    (if
     (insta/failure? ast)
      (do
        (println "ERROR: " ast)
        (interactive state))
      (let [command (ffirst ast)]
        (case command
          :EXIT (exit)
          :LIST (list-loaded-classes state)
          :HELP (help state)
          :LOAD (load-classes ast)
          :MC (let [threshold (read-string (last (last (first ast))))]
                (mc-operation state threshold))
          :MCP (let [threshold (read-string (last (last (first ast))))]
                 (mcp-operation state threshold))
          :F (f-operation state)
          :ST (st-operation state)
          (command-not-implemented command state))))))

(defn interactive [state]
  (do
    (print "> ")
    (flush)
    (let [user-input (read-line)]
      (if (empty? user-input)
        (interactive state)
        (process state user-input)))))
