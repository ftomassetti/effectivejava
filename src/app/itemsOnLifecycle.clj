(ns app.itemsOnLifecycle
  (:use [app.model.protocols])
  (:use [app.model.javaparser])
  (:use [app.operations])
  (:use [app.utils])
  (:use [app.javaparser.navigation])
  (:use [app.symbol_solver.funcs])
  (:use [app.symbol_solver.type_solver])
  (:import [app.operations Operation]))

; ============================================
; ITEM 1
; ============================================

; [ {:name "ClassQName", :len 75 :cutOn :onStart} {:name "n", :len 3}])]

(defn classesWithManyConstructors
  "Classes which have threshold or more not private constructors"
  [params]
  (filter
   (fn [v] (>= (nth v 1) (:threshold params)))
   (map
    (fn [cl] [cl (nNotPrivateConstructors cl)])
    (allClassesForCus (:cus params)))))

(def classesWithManyConstructorsOp
  (Operation.
   classesWithManyConstructors
   [:threshold]
   [:class :numberOfConstructors]))

; ============================================
; ITEM 2
; ============================================

(defn constructorsWithManyParameters
  "The non private constructors which takes threshold or more parameters"
  [params]
  (filter
   (fn [v] (>= (nth v 1) (:threshold params)))
   (map
    (fn [constructor]
      (let [np (.size (getParameters constructor))]
        [constructor np]))
    (allConstructorsForCus (:cus params)))))

(def constructorsWithManyParametersOp
  (Operation.
   constructorsWithManyParameters
   [:threshold]
   [:constructor :numberOfParameters]))

; ============================================
; ITEM 3
; ============================================

(defn isPublicFieldSingleton? [cl]
  (seq
   (filter
    (fn [f]
      (and
       (isPublicOrHasPackageLevelAccess? f)
       (isStatic? f)
       (= (getName f) "INSTANCE")))
    (getFieldsVariablesTuples cl))))

(defn isPublicMethodSingleton? [cl]
  (seq
   (filter
    (fn [m]
      (and
       (isPublicOrHasPackageLevelAccess? m)
       (isStatic? m)
       (= (getName m) "getInstance")))
    (getMethods cl))))

(defn isSingletonEnum? [e]
  (and
   (=
    1
    (.size
     (.getEntries e)))
   (=
    "INSTANCE"
    (getName
     (first
      (.getEntries e))))))

(defn getSingletonType
  "Return the singleton type or nil: :publicField :getInstance "
  [t]
  (cond
    (and
     (isClass? t)
     (isPublicFieldSingleton? t)) :publicField
    (and
     (isClass? t)
     (isPublicMethodSingleton? t)) :staticFactory
    (and
     (isEnum? t)
     (isSingletonEnum? t)) :singletonEnum
    :else nil))

(defn classesAndSingletonType
  "The type of singleton implemented in a certain class"
  [params]
  (filter
   (fn [v] (not-nil? (nth v 1)))
   (map
    (fn [cl] [cl (getSingletonType cl)])
    (allClassesForCus (:cus params)))))

(def classesAndSingletonTypeOp
  (Operation.
   classesAndSingletonType
   []
   [:class :singletonType]))

; ============================================
; ITEM 4
; ============================================

(defn isUtilClass?
  [cl]
  (let [ms (getMethods cl)]
    (and
     (pos? (count ms))
     (every? isStatic? ms))))

(defn utilsClasses
  "Find all Utils classes"
  [cus]
  (let [classes (flatten (map allClasses cus))]
    (filter isUtilClass? classes)))

; there should be exactly one constructor
; the constructor should take no params
; the constructor should be private
(defn hasOnlyOnePrivateConstructorTakingNoParams [clazz]
  (let [constructors (getConstructors clazz)]
    (if (= 1 (count constructors))
      (let [c (first constructors)]
        (and
         (isPrivate? c)
         (zero? (count (getParameters c)))))
      false)))

(defn utilClassProblem
  "Return a problem found about a utils class or nil, if none can be found"
  [utilClass]
  (when-not (hasOnlyOnePrivateConstructorTakingNoParams utilClass)
    "The class should have only one single private constructor taking no params"))

(defn- utilsClassesQuery [params]
  (let [{cus :cus onlyIncorrect :onlyIncorrect} params
        clazzes (utilsClasses cus)
        clazzesAndProblems (map (fn [cl] [cl (utilClassProblem cl)]) clazzes)]
    (if onlyIncorrect
      (filter
       (fn [tuple] (not (nil? (nth tuple 1))))
       clazzesAndProblems)
      clazzesAndProblems)))

; This operation can return either all the utils classes or only the utils classes with problems (depending on
; the param :onlyIncorrect
; Either way it produces a table with two columns: class and problem. Problem can be potentially empty.
(def utilsClassesOp
  (Operation. utilsClassesQuery [:onlyIncorrect] [:class :problem]))

; ============================================
; ITEM 7
; ============================================

(defn- isNilOrEmpty?
  "JavaParser could return either nil or an empty list in certain cases"
  [list]
  (or (nil? list) (empty? list)))

(defn calls-finalizers? [class]
  (pos? (count
         (filter #(and (= "finalize" (.getName %))
                       (isNilOrEmpty? (.getArgs %)))
                 (getMethodCallExprs class)))))

(defn classes-using-finalizers [params]
  (let [classes (flatten (map allClasses (:cus params)))]
    (map #(vec (list % nil))
         (filter calls-finalizers? classes))))

(def finalizersOp
  (Operation.
   classes-using-finalizers
   []
   [:class]))

; ============================================
; ITEM 10
; ============================================

(defn overrides-toString? [class]
  (->> (getMethods class)
       (filter #(= (getName %) "toString"))
       (filter #(empty? (getParameters %)))
       (count)
       (= 1)))

(defn hierarchy-overrides-toString? [type-solver-classes class]
  (binding [typeSolver (typeSolverOnList type-solver-classes)]
    (->> (conj (getAllSuperclasses class) class)
         (some overrides-toString?)
         (true?))))

(defn classes-that-do-not-override-toString [params]
  (let [classes (flatten (map allClasses (:cus params)))]
    (map #(vec (list % nil))
         (filter #(complement (hierarchy-overrides-toString? classes %))
                 classes))))

(def toStringOp
  (Operation.
    classes-that-do-not-override-toString
    []
    [:class]))
