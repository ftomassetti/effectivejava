(ns app.operations
  (:use [app.model.protocols])
  (:use [app.model.javaparser]))

(import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration)
(import com.github.javaparser.ast.body.ConstructorDeclaration)

(defrecord Operation [query params headers])

(derive ::integer ::printable)

(defmulti toString class)

(defmethod toString :default [x]
  (str x))

(defmethod toString String [x]
  x)

(defmethod toString clojure.lang.Keyword [x]
  (name x))

(defmethod toString ClassOrInterfaceDeclaration [x]
  (getQName x))

(defmethod toString ConstructorDeclaration [x]
  (getQName x))

(defn resultToStrings [resultRow]
  (vec (map toString resultRow)))

(defn resultsToStrings [results]
  (map resultToStrings results))

(defn getN [l ind]
  (if (zero? ind)
    (first l)
    (getN (rest l) (dec ind))))

(def columnSeparator " | ")

(defn columnLength [headerStr resultsStrs ind]
  (max
   (.length headerStr)
   (apply max
          (into [0] (map (fn [row] (.length (nth row ind))) resultsStrs)))))

(defn columnLengthsHelper [headersStrings resultsStrings acc ind]
  (if (empty? headersStrings)
    acc
    (columnLengthsHelper
     (rest headersStrings)
     resultsStrings
     (conj
      acc
      (columnLength (first headersStrings) resultsStrings ind))
     (inc ind))))

(defn columnLengths [headersStrings, resultsStrings]
  (vec (columnLengthsHelper headersStrings resultsStrings [] 0)))

(defn padStr [s len]
  (if (>= (.length s) len)
    s
    (padStr (str s " ") len)))

(defn rowStr [lengths values]
  (let
   [paddedValues (map (fn [l v] (padStr v l)) lengths values)]
    (clojure.string/join columnSeparator paddedValues)))

(defn sum [v]
  (apply + v))

(defn separatorStr [lengths]
  (let [sumLenRows (sum lengths)
        sumLenSeparators (* (dec (.length lengths)) (.length columnSeparator))
        n (+ sumLenRows sumLenSeparators)]
    (clojure.string/join (repeat n "-"))))

(defn printTable [headers results]
  (let [resultsStrings (resultsToStrings results)
        headersStrings (resultToStrings headers)
        colLengths (columnLengths headersStrings resultsStrings)]
    (println (rowStr colLengths headersStrings))
    (println (separatorStr colLengths))
    (doall (for [r resultsStrings]
             (println (rowStr colLengths r))))))

(defn printOperation [operation cus threshold]
  (let [headers (.headers operation)
        results ((.query operation) {:cus cus :threshold threshold})]
    (printTable headers results)))

; =============================================
; Collector protocols
; =============================================

; A ResultCollector receive the description of the fields,
; could perform some action and should return a ParamCollector
(defprotocol ResultCollector
  (header [this fields]))

; A ParamCollector do some operation on the table
(defprotocol ParamCollector
  (row [this values]))

; =============================================
; TablePrinter Collector
; =============================================

; Helping functions

(defn- cutString [s cutOn]
  (cond
    (= :onStart cutOn)
    (subs s 1)
    (= :onEnd cutOn)
    (try
      (subs s 0 (dec (.length s)))
      (catch Exception e (throw (Exception. "Caspita..."))))
    :else
    (throw (Exception. "cutOn should be :onStart or :onEnd"))))

(defn formatValue [v len cutOn]
  (cond
    (< (.length v) len)
    (formatValue (str v " ") len cutOn)
    (> (.length v) len)
    (formatValue (cutString v cutOn) len cutOn)
    :else v))

(defn fieldValueToStr [f v]
  (let [formattedValue (formatValue (str v) (:len f) (or (:cutOn f) :onEnd))]
    (str formattedValue " | ")))

(defn fieldValuesToStr [fields values]
  (if (empty? fields) ""
      (let [f (first fields), v (first values)]
        (str
         (fieldValueToStr f v)
         (fieldValuesToStr (rest fields) (rest values))))))

; it has not state
(defrecord TablePrinter [])

(defrecord TableRowPrinter [fields])

(def printer (TablePrinter.))

(defn printParam [printer p]
  (let
   [fmtStr (str "%-" (dec (:len p)) "s")]
    (print (format fmtStr (:name p)) " | ")))

(defn printSeparator [printer params]
  (doseq [p params]
    (print (clojure.string/join (repeat (+ (:len p) 2) "-"))))
  (println "-"))

(extend-protocol ResultCollector TablePrinter
                 (header [this fields]
                   (do
                     (doseq [p fields]
                       (printParam this p))
                     (println "")
                     (printSeparator this fields)
                     (TableRowPrinter. fields))))

(extend-protocol ParamCollector TableRowPrinter
                 (row [this values]
                   (do
                     (print (fieldValuesToStr (.fields this) values))
                     (println ""))))