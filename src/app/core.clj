(ns app.core
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:use [app.linter])
  (:use [app.interactive])
  (:use [app.cli])
  (:gen-class :main true))

(def self-exclusive-modes-error
  "Linter, interactive and query mode are self exclusive")

(defn usageError [banner opts msg]
  (if (clojure.string/blank? msg)
    (println (str "Incorrect usage"))
    (println (str "Incorrect usage: " msg)))
  (when (:errors opts)
    (doseq [e (:errors opts)]
      (println " * " e)))
  (println banner))

(defn info [msg]
  (println " [info] " msg))

(defn conflicting-options?
  "Return true if opts contains self-exclusive modes"
  [opts]
  (let [self-exclusive-modes [:linter :interactive :query]]
    (->> (select-keys opts self-exclusive-modes)
         (vals)
         (filter identity)
         (count)
         (< 1))))


;; The following methods have been extracted from the main function.
;; These methods should probably be moved to a different namespace.

(defn treat-possible-errors [opts banner]
  (when (:errors opts)
    (usageError banner opts "")
    (System/exit 1))
  (when (conflicting-options? opts)
    (usageError banner opts self-exclusive-modes-error)
    (System/exit 1)))

(defn run-linter-mode [opts]
  (when-not (:dir opts)
    (info "Linter, no directory indicated. Using current directory")
    (linter ".")
    (System/exit 0))
  (linter (:dir opts))
  (System/exit 0))

(defn run-interactive-mode []
  (interactive {})
  (System/exit 0))

(defn show-help [banner]
  (println "Printing help message, as asked")
  (println banner)
  (System/exit 0))

(defn run-query-mode [opts banner]
  (if
   (and
    (:dir opts)
    (:query opts)
    ((keyword (:query opts)) operations)
    (nil? (:errors opts)))
    (run opts)
    (do (usageError banner opts "")
        (System/exit 1))))

(defn -main
  [& args]
  (let [optsMap (parse-opts args cliOpts)
        opts (:options optsMap)
        banner (:summary optsMap)]
    (treat-possible-errors opts banner)
    (cond
      (:linter opts) (run-linter-mode opts)
      (:interactive opts) (run-interactive-mode)
      (:help opts) (show-help banner)
      :else (run-query-mode opts banner))))
