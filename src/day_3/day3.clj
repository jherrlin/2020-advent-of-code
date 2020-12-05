(ns user)


(count ".............#...#....#.....##.")

(reduce
 (fn [[trees x] line]
   [(if (= \# (->> (mod x 31) (nth line)))
      (inc trees) trees)
    (+ x 3)])
 [0 0]
 (clojure.string/split (slurp "input.txt") #"\n"))

; => [167 969]




(def test-input
  "..##.......
   #...#...#..
   .#....#..#.
   ..#.#...#.#
   .#...##..#.
   ..#.##.....
   .#.#.#....#
   .#........#
   #.##...#...
   #...##....#
   .#..#...#.#")


(->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
     (map
      (fn [[x* y*]]
        (reduce
         (fn [[trees x toggle] line]
           [(if (and (= \# (->> (mod x (count line)) (nth line)))
                     (if (= y* 1) true toggle))
              (inc trees) trees)
            (cond (= y* 1)              (+ x x*)
                  (and (= y* 2) toggle) (+ x x*)
                  :else                 x)
            (not toggle)])
         [0 0 true]
         (clojure.string/split (slurp "input.txt") #"\n"))))
     (map first)
     (apply *))

;; 736527114
