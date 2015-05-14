(ns app.model.reflection
  (:use [app.utils])
  (:use [app.model.protocols])
  (:use [app.javaparser.parsing])
  (:use [app.javaparser.navigation]))

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