(ns effectivejava.model.reflection
  (:use [effectivejava.utils])
  (:use [effectivejava.model.protocols])
  (:use [effectivejava.javaparser.parsing])
  (:use [effectivejava.javaparser.navigation]))

; Needed only for system classes

; ============================================
; Naming
; ============================================

(extend-protocol Named
  java.lang.Class
  (getName [this]
    (.getSimpleName this))
  (getQName [this]
    (.getCanonicalName this)))

; ============================================
; TypeRef
; ============================================

(extend-protocol TypeRef
  java.lang.Class
  (array? [this] false))