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
(load "operations")
(load "itemsOnLifecycle")
(load "interactive")
(load "linter")
(load "cli")

(def self-exclusive-modes-error
  "Linter, interactive and query mode are self exclusive")

(defn usageError [banner opts msg]
  (println (str "Incorrect usage: " msg))
  (when (:errors opts)
    (doseq [e (:errors opts)]
      (println " * " e)))
  (println banner)
  (System/exit 1))

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
    (usageError banner opts ""))
  (when (conflicting-options? opts)
    (usageError banner opts self-exclusive-modes-error)))

(defn run-linter-mode [opts]
  (when (not (:dir opts))
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
      (name2operation (:query opts))
      (nil? (:errors opts)))
    (run opts)
    ;; The following code block is very similar to the usageError function.
    ;; Try to call that method to avoid repeating code.
    (do
      (println "Incorrect usage")
      (when (:errors opts)
        (doseq [e (:errors opts)]
          (println " * " e)))
      (println banner)
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
