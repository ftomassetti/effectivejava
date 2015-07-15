(ns effectivejava.test.utils
  (:use [clojure.test])
  (:use [effectivejava.utils]))

; ============================================
; preceedingChildren
; ============================================

(deftest testPreceedingChildren
  (is (= '(1 2 3 4) (preceedingChildren [1 2 3 4 5] 5)))
  (is (= '(1 2) (preceedingChildren [1 2 3 4 5] 3)))
  (is (= '() (preceedingChildren [1 2 3 4 5] 1))))
