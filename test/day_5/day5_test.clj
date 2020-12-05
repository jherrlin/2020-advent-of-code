(ns day-5.day5-test
  (:require [day-5.day5 :as sut]
            [clojure.test :as t]))


(t/deftest find*
  (t/is (= (sut/find* "BFFFBBF" 0 128)
           70))

  (t/is (= (sut/find* "FFFBBBF" 0 128)
           14))

  (t/is (= (sut/find* "BBFFBBF" 0 128)
           102))

  (t/is (= (sut/find* "RRR" 0 8)
           7))

  (t/is (= (sut/find* "LLL" 0 8)
           0))

  (t/is (= (sut/find* "RLL" 0 8)
           4))

  (t/is (= (sut/find-it "BFFFBBFRRR")
           567))

  (t/is (= (sut/find-it "FFFBBBFRRR")
           119))

  (t/is (= (sut/find-it "BBFFBBFRLL")
           820))

  )
