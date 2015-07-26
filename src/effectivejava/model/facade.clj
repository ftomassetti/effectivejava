(ns effectivejava.model.facade
  (:use [potemkin])
  (:import [effectivejava.model.protocols])
  (:import [effectivejava.model.javaparser])
  (:import [effectivejava.model.refelction])
  (:import [effectivejava.model.javassist]))

(import-vars [effectivejava.model.protocols isClass? isInterface? isEnum? isNotPrivate? getName getQName packageName allFields fieldName isInDefaultPackage?])