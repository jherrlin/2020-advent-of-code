(ns day-5.day5-test
  (:require [day-5.day5 :as sut]
            [clojure.test :as t]))


(t/deftest find-row
  (t/is (= (sut/find-row "BFFFBBF" 0 128)
           70))

  (t/is (= (sut/find-row "FFFBBBF" 0 128)
           14))

  (t/is (= (sut/find-row "BBFFBBF" 0 128)
           102))

  )
