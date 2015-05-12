(ns app.javassist
  (:use [app.utils])
  (:use [app.javaparser]))

; ============================================
; Naming
; ============================================

(extend-protocol Named
  ; javassist.CtClassType is a package protected sublass of CtClass
  javassist.CtClass
  (getName [this]
    (.getSimpleName this))
  (getQName [this]
    (.getName this)))