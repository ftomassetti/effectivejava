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

(defn usageError [banner opts msg]
  (println (str "Incorrect usage: " msg))
  (when (:errors opts)
    (doseq [e (:errors opts)]
      (println " * " e)))
  (println banner)
  (System/exit 1))

(defn info [opts msg]
  (println " [info] " msg))

; TODO extract part of this method to cli
(defn -main
  "What I do"
  [& args]
  (let [optsMap
    (parse-opts args cliOpts)
    opts (:options optsMap)
    banner (:summary optsMap)]
    (do
      (when (:errors opts)
        (usageError banner opts ""))
      (when (:linter opts)
        (when (or (:interactive opts) (:query opts))
          (usageError banner opts "Linter, interactive and query mode are self exclusive"))
        (when (not (:dir opts))
          (info opts "Linter, no directory indicated. Using current directory")
          (linter ".")
          (System/exit 0))
        (linter (:dir opts))
        (System/exit 0))
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
          (name2operation (:query opts))
          (nil? (:errors opts)))
        (run opts)
        (do
          (println "Incorrect usage")
          (when (:errors opts)
            (doseq [e (:errors opts)]
              (println " * " e)))
          (println banner)
          (System/exit 1))))))
