(ns day-8.day8-test
  (:require [day-8.day8 :as sut]
            [clojure.test :as t]))



(t/deftest problem-one
  (t/is (= (second (sut/p-run -1))
           1816))

  (->> (sut/p-run -1)
       last
       (filter (comp #{:jmp :nop} second))
       (count)
       (range)
       (map #(sut/p-run %))
       (filter (comp true? first))
       ((comp second first))
       (= 1149)
       (t/is))
  )
