(deftest testFormatValueNoCutOnStartNeeded
  (is (= "short     " (formatValue "short" 10 :onStart))))

(deftest testFormatValueCutOnStartNeeded
  (is (= "ingToBeCut" (formatValue "aLongStringToBeCut" 10 :onStart))))

(deftest testFormatValueNoCuOnEndtNeeded
  (is (= "short     " (formatValue "short" 10 :onEnd))))

(deftest testFormatValueCutOnEndNeeded
  (is (= "aLongStrin" (formatValue "aLongStringToBeCut" 10 :onEnd))))