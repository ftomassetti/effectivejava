; ============================================
; ITEM 1
; ============================================

; [ {:name "ClassQName", :len 75 :cutOn :onStart} {:name "n", :len 3}])]

(defn classesWithManyConstructors
  "Print the classes which have threshold or more not private constructors"
  [cus threshold]
  (filter
    (fn [m] (>= (nth m 1) threshold))
    (map
      (fn [cl] [cl (nNotPrivateConstructors cl)])
      (allClassesForCus cus))))

(def classesWithManyConstructorsOp
  (Operation.
    "classesWithManyConstructors"
    "mc"
    classesWithManyConstructors
    [:threshold]
    [:class :numberOfConstructors]))

(defn printClassesWithManyConstructors
  "Print the classes which have threshold or more not private constructors"
  [cus threshold]
  (printOperation classesWithManyConstructorsOp cus threshold))

; ============================================
; ITEM 2
; ============================================

(defn printConstructorsWithManyParameters [cus threshold]
  "Print the not private constructors which takes threshold or more parameters"
  (doseq [cl (allClassesForCus cus)]
    (doseq [cs (getNotPrivateConstructors cl)]
      (let [np (.size (getParameters cs))]
        (if (>= np threshold)
          (println (getQName cl) "." cs " : " np)
          nil)))))

; ============================================
; ITEM 3
; ============================================

(defn isPublicFieldSingleton? [cl]
  (not (empty?
         (filter
           (fn [f]
             (and
               (isPublicOrHasPackageLevelAccess? f)
               (isStatic? f)
               (= (getName f) "INSTANCE")))
           (getFieldsVariablesTuples cl)))))

(defn isPublicMethodSingleton? [cl]
  (not (empty?
         (filter
           (fn [m]
             (and
               (isPublicOrHasPackageLevelAccess? m)
               (isStatic? m)
               (= (getName m) "getInstance")))
           (getMethods cl)))))

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

(defn printSingletonType [cus threshold]
  "Print the not private constructors which takes threshold or more parameters"
  (doseq [cu cus]
    (doseq [t (.getTypes cu)]
      (let [st (getSingletonType t)]
        (if (not-nil? st)
          (println (getQName t) " : " st)
          nil)))))