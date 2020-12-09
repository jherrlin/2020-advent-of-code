(ns day-7.day7-test
  (:require [day-7.day7 :as sut]
            [clojure.test :as t]))


(t/deftest first-test
  (t/is (= (->> (keys sut/bags)
                (map (partial sut/inspect-bag-chain "shiny gold"))
                (apply clojure.set/union)
                (remove #{"shiny gold"})
                (count))
           300)))

(t/deftest second-test
  (t/is (= (first (sut/walk-the sut/bags (get sut/bags "shiny gold")))
         8030)))
