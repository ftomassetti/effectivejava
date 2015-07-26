(ns effectivejava.jarloading
  (:use [effectivejava.model.facade])
  (:use [effectivejava.operations])
  (:use [effectivejava.utils])
  (:import [effectivejava.operations Operation]))

(import java.net.URLDecoder)
(import java.util.jar.JarEntry)
(import java.util.jar.JarFile)
(import javassist.ClassPool)
(import javassist.CtClass)

; An element on the classpath (a single class, interface, enum or resource file)
(defrecord ClasspathElement [resource path contentAsStreamThunk])

(defn- jarEntryToClasspathElement [jarFile jarEntry]
  (let [name (.getName jarEntry)
        content (fn [] (.getInputStream jarFile jarEntry))]
    (ClasspathElement. jarFile name content)))

(defn getElementsEntriesInJar
  "Return a set of ClasspathElements"
  [pathToJarFile]
  (let [url (URLDecoder/decode pathToJarFile "UTF-8")
        jarfile (new JarFile url)
        entries (enumeration-seq (.entries jarfile))
        entries' (filter (fn [e] (not (.isDirectory e))) entries)]
    (map (partial jarEntryToClasspathElement jarfile) entries')))

(defn getClassesEntriesInJar
  "Return a set of ClasspathElements"
  [pathToJarFile]
  (filter (fn [e] (.endsWith (.path e) ".class")) (getElementsEntriesInJar pathToJarFile)))

(defn pathToTypeName [path]
  (if (.endsWith path ".class")
    (let [path' (.substring path 0 (- (.length path) 6))
          path'' (clojure.string/replace path' #"/" ".")
          path''' (clojure.string/replace path'' "$" ".")]
      path''')
    (throw (IllegalArgumentException. "Path not ending with .class"))))

(defn findEntry
  "return the ClasspathElement corresponding to the given name, or nil"
  [typeName classEntries]
  (first (filter (fn [e] (= typeName (pathToTypeName (.path e)))) classEntries)))

(defn findType
  "return the CtClass corresponding to the given name, or nil"
  [typeName classEntries]
  (let [entry (findEntry typeName classEntries)
        classPool (ClassPool/getDefault)]
    (when entry
      (.makeClass classPool ((.contentAsStreamThunk entry))))))
