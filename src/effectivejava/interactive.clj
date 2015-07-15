(ns effectivejava.interactive
  (:use [effectivejava.model.protocols]
        [effectivejava.model.javaparser]
        [effectivejava.javaparser.navigation]
        [effectivejava.operations]
        [effectivejava.itemsOnLifecycle]
        [effectivejava.utils])
  (:require [instaparse.core :as insta])
  (:import [effectivejava.operations Operation]))

; ============================================
; Interactive mode
; ============================================

(def commands-grammar
  (clojure.string/join
   "\n"
   ["<COMMAND> = HELP | EXIT | LOAD | LIST | MC | MCP | F | ST | TS"
    "HELP = 'help' | 'h'"
    "EXIT = 'exit' | 'quit' | 'q'"
    "LOAD = 'load' <WS> STR"
    "LIST = 'list'"
    "MC  = ('mc'|'many-constructors') <WS> 'th' <WS> NUM"
    "MCP = ('mcp'|'many-costructor-params') <WS> 'th' <WS> NUM"
    "F = ('f'|'finalizers')"
    "ST = ('st'|'singletons')"
    "TS = ('ts'|'toString')"
    "WS  = #'[\t ]+'"
    "NUM = #'[0-9]+'"
    "STR = #'\"[^\"]*\"'"]))

(def command-parser
  (insta/parser commands-grammar))

(def no-classes-loaded-error
  "No classes loaded. Use <load> first")

(def help-message
  (clojure.string/join
   "\n"
   ["h/help                             : print this help message"
    "q/quit/exit                        : close the shell"
    "list                               : list classes loaded"
    "load DIR                           : load classes from DIR"
    "mc/many-constructors th NUM        : list classes with NUM or more constructors"
    "mcp/many-constructor-params th NUM : list constructors with NUM or more parameters"
    "f/finalizers                       : list classes that use finalizers"
    "st/singletons                      : list singletons"
    "ts/toString                        : list classes that do not override toString()"]))

(def exit-message
  "Exit...")

(def operation-commands
  {:MC classesWithManyConstructorsOp
   :MCP constructorsWithManyParametersOp
   :F finalizersOp
   :ST classesAndSingletonTypeOp
   :TS toStringOp})

(declare interactive)

(defn- exit []
  (println exit-message))

(defn- list-loaded-classes [state]
  (let [loadedCus (:cus state)]
    (if loadedCus
      (do
        (println "Listing types currently loaded:")
        (doseq [cu (:cus state)]
          (doseq [t (.getTypes cu)]
            (println " *" (getQName t)))))
      (println no-classes-loaded-error))
    (interactive state)))

(defn- show-help [state]
  (println help-message)
  (interactive state))

(defn- load-classes [ast]
  (let [dirnameWithApex (nth (nth (first ast) 2) 1)
        dirname (subs dirnameWithApex 1 (dec (count dirnameWithApex)))]
    (println "Loading" dirname)
    (let [loadedCus (cus dirname)]
      (println "Java files loaded:" (count loadedCus))
      (interactive {:cus loadedCus}))))

(defn- command-not-implemented [command state]
  (println "Command not implemented: " command)
  (interactive state))

(defn- operation [op-command state & [threshold]]
  (printOperation (op-command operation-commands) (:cus state) threshold)
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
          :HELP (show-help state)
          :LOAD (load-classes ast)
          :MC (let [threshold (read-string (last (last (first ast))))]
                (operation :MC state threshold))
          :MCP (let [threshold (read-string (last (last (first ast))))]
                 (operation :MCP state threshold))
          :F (operation :F state)
          :ST (operation :ST state)
          :TS (operation :TS state)
          (command-not-implemented command state))))))

(defn interactive [state]
  (print "> ")
  (flush)
  (let [user-input (read-line)]
    (if (empty? user-input)
      (interactive state)
      (process state user-input))))
