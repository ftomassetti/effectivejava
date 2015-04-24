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
