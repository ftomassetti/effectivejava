(ns app.javassist
  (:use [app.utils])
  (:use [app.model.protocols]))

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

;
; protocol fieldDecl
;

(extend-protocol FieldDecl
  javassist.CtField
  (fieldName [this]
    (.getName this)))