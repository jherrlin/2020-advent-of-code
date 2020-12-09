(ns day-9.day9-test
  (:require [day-9.day9 :as sut]
            [clojure.test :as t]))

(t/deftest calculate
  (t/is (= (sut/calculate sut/values 0 25)
           1212510616)))
