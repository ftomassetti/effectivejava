(ns effectivejava.utils)

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

; ============================================
; Collections
; ============================================

(defn find-index [elmts elmt]
  (if (empty? elmts)
    -1
    (if (identical? (first elmts) elmt)
      0
      (let [rest (find-index (rest elmts) elmt)]
        (if (= -1 rest)
          -1
          (inc rest))))))

(defn preceedingChildren [children child]
  (let [i (find-index children child)]
    (if (= -1 i)
      (throw (RuntimeException. "Not found"))
      (take i children))))