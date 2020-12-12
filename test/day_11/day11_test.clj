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

(t/deftest next-w
  (let [gen [[0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]]]
    (t/is (= (sut/next-w [2 2] gen)
             [1 2]))
    (t/is (= (sut/next-w [1 2] gen)
             [0 2]))
    (t/is (= (sut/next-w [0 0] gen)
             nil))
    (t/is (= (sut/next-w [55 55] gen)
             nil))))

(t/deftest next-nw
  (let [gen [[0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]]]
    (t/is (= (sut/next-nw [2 2] gen)
             [1 1]))
    (t/is (= (sut/next-nw [1 2] gen)
             [0 1]))
    (t/is (= (sut/next-nw [0 0] gen)
             nil))
    (t/is (= (sut/next-nw [55 55] gen)
             nil))))

(t/deftest next-n
  (let [gen [[0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]]]
    (t/is (= (sut/next-n [2 2] gen)
             [2 1]))
    (t/is (= (sut/next-n [1 2] gen)
             [1 1]))
    (t/is (= (sut/next-n [0 0] gen)
             nil))
    (t/is (= (sut/next-n [55 55] gen)
             nil))))

(t/deftest next-ne
  (let [gen [[0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]]]
    (t/is (= (sut/next-ne [2 2] gen)
             [3 1]))
    (t/is (= (sut/next-ne [1 2] gen)
             [2 1]))
    (t/is (= (sut/next-ne [4 0] gen)
             nil))
    (t/is (= (sut/next-ne [55 55] gen)
             nil))))

(t/deftest next-e
  (let [gen [[0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]]]
    (t/is (= (sut/next-e [2 2] gen)
             [3 2]))
    (t/is (= (sut/next-e [1 2] gen)
             [2 2]))
    (t/is (= (sut/next-e [4 0] gen)
             nil))
    (t/is (= (sut/next-e [55 55] gen)
             nil))))

(t/deftest next-se
  (let [gen [[0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]]]
    (t/is (= (sut/next-se [2 2] gen)
             [3 3]))
    (t/is (= (sut/next-se [1 2] gen)
             [2 3]))
    (t/is (= (sut/next-se [4 0] gen)
             nil))
    (t/is (= (sut/next-se [55 55] gen)
             nil))))

(t/deftest next-s
  (let [gen [[0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]]]
    (t/is (= (sut/next-s [2 2] gen)
             [2 3]))
    (t/is (= (sut/next-s [1 2] gen)
             [1 3]))
    (t/is (= (sut/next-s [2 4] gen)
             nil))
    (t/is (= (sut/next-s [55 55] gen)
             nil))))


(t/deftest next-sw
  (let [gen [[0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]
             [0 0 0 0 0]]]
    (t/is (= (sut/next-sw [2 2] gen)
             [1 3]))
    (t/is (= (sut/next-sw [1 2] gen)
             [0 3]))
    (t/is (= (sut/next-sw [0 4] gen)
             nil))
    (t/is (= (sut/next-sw [55 55] gen)
             nil))))
