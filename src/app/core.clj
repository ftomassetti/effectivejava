(ns app.core)

(import japa.parser.JavaParser)
(import japa.parser.ast.body.ClassOrInterfaceDeclaration)
(import  japa.parser.ast.body.ConstructorDeclaration)

(defn parse [filename]
  (parseFile (new java.io.File filename)))

(defn parseFile [file]
  (try
    (JavaParser/parse file)
    (catch Exception e nil)))

(defn parseAllFiles [files]
  (map (fn [f] [f (parseFile f)]) files))

(defn parseAllDir [dirname]
  (parseAllFiles (java-files dirname)))

(defn java-file? [f]
  (and (.isFile f)
    (.endsWith (.getName f) ".java")))

(defn java-files [dirname]
  (let [d (java.io.File. dirname)]
    (filter java-file? (file-seq d))))

(defn parseDir [dirname]
  (let [d (java.io.File. dirname)]
    (doseq [f (.listFiles d)]
      (if (.isDirectory f)
        (print "d ")
        (print "- "))
      (println (.getName f)))))

(defn isClass? [n]
  (and
    (instance? ClassOrInterfaceDeclaration n)
    (not (.isInterface n))))

; Consider internal classes
(defn getClasses [cu]
  (filter isClass? (.getTypes cu)))

(def not-nil? (complement nil?))

(def filename "/home/federico/repos/javaparser/src/main/java/japa/parser/Position.java")
(def dirname "/home/federico/repos/javaparser/src")
; Get tuples of [filename cu]
(def cusTuples
  (filter not-nil? (parseAllDir dirname)))
(def cus
  (map (fn [[_ cu]] cu) cusTuples))

(defn getConstructors [cl]
  (filter (fn [m] (instance? ConstructorDeclaration m)) (.getMembers cl)))

(defn nConstructors [cl]
  (.size (getConstructors cl)))


(defn constructors [classDecl]
  )