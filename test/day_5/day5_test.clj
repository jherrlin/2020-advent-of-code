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

  (t/is (= (sut/find-row "RRR" 0 8)
           7))

  (t/is (= (sut/find-row "LLL" 0 8)
           0))

  (t/is (= (sut/find-row "RLL" 0 8)
           4))

  (t/is (= (sut/find-it "BFFFBBFRRR")
           567))

  (t/is (= (sut/find-it "FFFBBBFRRR")
           119))

  (t/is (= (sut/find-it "BBFFBBFRLL")
           820))

  )
