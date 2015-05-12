(ns app.model.protocols
  (:use [app.javaparser])
  (:use [app.operations])
  (:use [app.itemsOnLifecycle])
  (:use [app.utils])
  (:use [app.symbol_solver.type_solver])
  (:require [instaparse.core :as insta])
  (:import [app.operations Operation]))

(defprotocol TypeRef
  "Reference to a type"
  (array? [this])
  (primitive? [this])
  (typeName [this])
  (baseType [this]))

(defprotocol TypeDecl
  "Defiinition of a type (a Class, an Interface or an Enum)"
  (allFields [this]))

(defprotocol FieldDecl
  "Definition of Class, Enum or Interface Field. In the case of interface the field can only be static"
  (fieldName [this]))