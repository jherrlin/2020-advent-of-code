(ns day-11.day11-2-test
  (:require [day-11.day11-2 :as sut]
            [clojure.test :as t]))


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

(t/deftest look-around
  (->> ".##.##.
#.#.#.#
##...##
...L...
##...##
#.#.#.#
.##.##."
       (sut/parse-input)
       (sut/look-around [3 3])
       (every? #{:free})
       (t/is))


  (->> ".............
.L.L.#.#.#.#.
............."
       (sut/parse-input)
       (sut/look-around [1 1])
       (every? #{:free})
       (t/is))

  (->> ".......#.
...#.....
.#.......
.........
..#L....#
....#....
.........
#........
...#....."
       (sut/parse-input)
       (sut/look-around [3 4])
       (every? #{:occupied})
       (t/is))
   )


(def g1
"L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(def g2
"#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##")

(def g3
"#.LL.LL.L#
#LLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLL#
#.LLLLLL.L
#.LLLLL.L#")

(def g4
"#.L#.##.L#
#L#####.LL
L.#.#..#..
##L#.##.##
#.##.#L.##
#.#####.#L
..#.#.....
LLL####LL#
#.L#####.L
#.L####.L#")

(def g5
"#.L#.L#.L#
#LLLLLL.LL
L.L.L..#..
##LL.LL.L#
L.LL.LL.L#
#.LLLLL.LL
..L.L.....
LLLLLLLLL#
#.LLLLL#.L
#.L#LL#.L#")

(->> g1
     (sut/parse-input)
     (sut/next-generation)
     (sut/next-generation)
     (sut/next-generation)
     (sut/next-generation)
     (sut/parse-back)
     (= g5))


(t/deftest next-generation
  (->> g1
       (sut/parse-input)
       (sut/next-generation)
       (sut/parse-back)
       (= g2)
       (t/is))

  (->> g1
       (sut/parse-input)
       (sut/next-generation)
       (sut/next-generation)
       (sut/parse-back)
       (= g3)
       (t/is))

  (->> g1
       (sut/parse-input)
       (sut/next-generation)
       (sut/next-generation)
       (sut/next-generation)
       (sut/parse-back)
       (= g4)
       (t/is))

  (->> g1
       (sut/parse-input)
       (sut/next-generation)
       (sut/next-generation)
       (sut/next-generation)
       (sut/next-generation)
       (sut/parse-back)
       (= g5)
       (t/is))
  )
