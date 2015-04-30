(ns app.utils)

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