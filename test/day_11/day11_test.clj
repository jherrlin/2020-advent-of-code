(ns day-11.day11-test
  (:require [day-11.day11 :as sut]
            [clojure.test :as t]))

(def test-g1 "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(def test-g2 "#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##")

(def test-stabilized "#.#L.L#.##
#LLL#LL.L#
L.#.L..#..
#L##.##.L#
#.#L.LL.LL
#.#L#L#.##
..L.L.....
#L#L##L#L#
#.LLLLLL.L
#.#L#L#.##")

(t/deftest parse
  (->> test-g1
       (sut/parse-input)
       (sut/parse-back)
       (= test-g1)
       (t/is)))

(t/deftest next-generation
  (->> test-g1
       (sut/parse-input)
       (sut/next-generation)
       (sut/parse-back)
       (= test-g2)
       (t/is)
       )
  )

(t/deftest stabilized
  (->> test-g1
       (sut/parse-input)
       (sut/next-generation)
       (sut/parse-back)
       (= test-g2)
       (t/is)
       )
  )
