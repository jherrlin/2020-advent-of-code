(ns day-9.day9-test
  (:require [day-9.day9 :as sut]
            [clojure.test :as t]))

(t/deftest fist-number
  (t/is (= (sut/fist-number sut/input-values 0 25)
           1212510616)))


(t/deftest problem-nr-2
  (t/is (= (->> (map (fn [window-size]
                       (sut/find-a-contiguous-set
                        sut/input-values
                        1212510616
                        0
                        window-size
                        window-size))
                     (range 2 (count sut/input-values)))
                (remove nil?)
                (first))
           171265123))
  )
