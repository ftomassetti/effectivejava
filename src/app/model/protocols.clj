(ns app.model.protocols
  (:use [app.javaparser])
  (:use [app.operations])
  (:use [app.itemsOnLifecycle])
  (:use [app.utils])
  (:use [app.symbol_solver.type_solver])
  (:require [instaparse.core :as insta])
  (:import [app.operations Operation]))

(defprotocol TypeRef
  (array? [this])
  (primitive? [this])
  (typeName [this])
  (baseType [this]))

(defprotocol TypeDef
  (allFields [this]))

(defprotocol FieldDecl
  (fieldName [this]))