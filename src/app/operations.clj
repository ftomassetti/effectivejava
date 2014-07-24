(defrecord operation [name shortcut query params])

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
      (subs s 0 (- (.length s) 1))
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
    [fmtStr (str "%-" (- (:len p) 1) "s")]
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