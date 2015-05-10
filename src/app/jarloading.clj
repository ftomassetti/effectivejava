(ns app.jarloading
  (:use [app.javaparser])
  (:use [app.operations])
  (:use [app.utils])
  (:import [app.operations Operation]))

(import java.net.URLDecoder)
(import java.util.jar.JarEntry)
(import java.util.jar.JarFile)

; An element on the classpath (a single class, interface, enum or resource file)
(defrecord ClasspathElement [resource path contentAsStream])

(defn- jarEntryToClasspathElement [jarFile jarEntry]
  (let [name (.getName jarEntry)
        content (.getInputStream jarFile jarEntry)]
    (ClasspathElement. jarFile name content)))

(defn getElementsEntriesInJar
  "Return a set of ClasspathElements"
  [pathToJarFile]
  (let [url (URLDecoder/decode pathToJarFile "UTF-8")
        jarfile (new JarFile url)
        entries (enumeration-seq (.entries jarfile))
        entries' (filter (fn [e] (not (.isDirectory e))) entries )]
    (map (partial jarEntryToClasspathElement jarfile) entries')))

(defn getClassesEntriesInJar
  "Return a set of ClasspathElements"
  [pathToJarFile]
  (filter (fn [e] (.endsWith (.path e) ".class")) (getElementsEntriesInJar pathToJarFile)))

(defn pathToTypeName [path]
  (if (.endsWith path ".class")
    (let [path' (.substring path 0 (- (.length path) 6))
          path'' (clojure.string/replace path' #"/" ".")]
      path'')
    (throw (IllegalArgumentException. "Path not ending with .class"))))

(defn findEntry [typeName classEntries]
  )
