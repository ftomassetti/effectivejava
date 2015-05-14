(ns app.model.protocols)

; In the namespace we define the protocols describing Java elements.
; We will then implement these protocols both using Javaparser ASTs and Javassist elements obtained by 
; loading JAR files.

(defprotocol TypeRef
  "Reference to a type. A TypeRef could be a primitive type or a reference type (enum, class, interface). 
  In the later case it could take type parameters (other TypeRefs). It could also be a TypeVariable, like in:
  
  class A<B> { } 
  
  where B is a TypeVariable. It could also be Wildcard Type, possibly with constraints."

  (array? [this])
  (primitive? [this])
  ; class, interface and enum are reference-type
  (reference-type? [this])
  (typeName [this])
  (baseType [this])
  ; represent the position of the reference, it is used when solving symbols
  ; because a reference to a class A could be related to different classes depending on the position
  ; of the reference
  (context [this]))

(defrecord RTypeRef [array primitive reference-type type-name base-type context]
  TypeRef
  (array? [this] array)
  (primitive? [this] primitive)
  (reference-type? [this] reference-type)
  (typeName [this] type-name)
  (baseType [this] base-type)
  (context [this] context))

(defn make-array-type-ref [type-ref]
  (RTypeRef. true false false nil type-ref (context type-ref)))

(defn make-reference-type-ref [qname context]
  (RTypeRef. false false true qname nil context))

(defn make-primitive-type-ref [qname context]
  (RTypeRef. false true false qname nil context))

(defprotocol TypeDecl
  "Defiinition of a type (a Class, an Interface or an Enum)"
  (isEnum? [this])
  (isClass? [this])
  (isInterface? [this])
  (allFields [this]))

(defprotocol SymbolRef
  (getType [this])
  (localVarRef? [this])
  (parameterRef? [this])
  (fieldRef? [this]))

(defprotocol FieldDecl
  "Definition of Class, Enum or Interface Field. In the case of interface the field can only be static"
  (fieldName [this]))

; ============================================
; Naming
; ============================================

(defprotocol WithPackageName
  (packageName [this]))

(defprotocol Named
  (getName [this])
  (getQName [this]))

(defn isInDefaultPackage? [this]
  (= "" (packageName this)))

; ============================================
; Modifiers
; ============================================

(defprotocol WithModifiers
  (isPrivate? [this])
  (isPublic? [this])
  (isProtected? [this])
  (isStatic? [this])
  (isFinal? [this]))

(defn hasPackageLevelAccess? [withModifiers]
  (not
   (or
    (isPublic? withModifiers)
    (isPrivate? withModifiers)
    (isProtected? withModifiers))))

(defn isPublicOrHasPackageLevelAccess? [withModifiers]
  (or
   (isPublic? withModifiers)
   (hasPackageLevelAccess? withModifiers)))

(defn isNotPrivate? [withModifiers]
  (complement isPrivate?))