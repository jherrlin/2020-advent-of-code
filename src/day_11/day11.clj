(ns day-11.day11)


(def g1-test "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")



(defn parse-input [s]
  (->> (clojure.string/split s #"\n")
       (mapv #(clojure.string/split % #""))))

(defn parse-back [v]
  (->> v
       (map clojure.string/join)
       (clojure.string/join "\n")))

(defn individual-state [[x y] generation]
  (get-in generation [y x]))

(defn neighbour-idxs [[^int x-index ^int y-index]]
  [(vec (range (- x-index 1) (+ x-index 2)))
   (vec (range (- y-index 1) (+ y-index 2)))])

(defn neighbour-states
  [[current-x current-y] generation [x-indexs y-indexs]]
  (vec (for [x x-indexs
             y y-indexs
             :when (not (= [current-x current-y] [x y]))]
         (individual-state [x y] generation))))

(defn neighbours
  "Get neighbours state."
  [current-x-y generation]
  (->> (neighbour-idxs   current-x-y)
       (neighbour-states current-x-y generation)))

(defn next-state [current occupied]
  (cond
    (and (#{"L"} current) (= occupied 0)) "#"
    (and (#{"#"} current) (< 3 occupied)) "L"
    :else                                 current))

(defn occupied    [v]          (count (filter #{"#"} v)))
(defn game-width  [generation] (count (first generation)))
(defn game-height [generation] (count generation))

(defn next-individual-idx [[x y] generation]
  (let [width  (game-width generation)
        height (game-height generation)]
    [(mod (inc x) width)
     (mod (+ y (quot (inc x) width)) height)]))

(defn update-in-gen [old-gen new-gen [x y]]
  (assoc-in new-gen
            [y x]
            (->> (neighbours [x y] old-gen)
                 (occupied)
                 (next-state (individual-state [x y] old-gen)))))

(defn next-generation [generation]
  (let [end-idx [(dec (game-width generation)) (dec (game-height generation))]]
    (loop [new-gen generation
           x-y   [0 0]]
      (if (= end-idx x-y)
        (update-in-gen generation new-gen x-y)
        (recur
         (update-in-gen generation new-gen x-y)
         (next-individual-idx x-y generation))))))

(defn stabilized [input]
  (loop [prev-gen nil
         this-gen input]
    (if (= prev-gen this-gen)
      (parse-back this-gen)
      (recur
       this-gen
       (next-generation this-gen)))))

;; Problem 1
(comment
  (->> (slurp "src/day_11/input.txt")
       (parse-input)
       (stabilized)
       (filter #{\#})
       (count))
  )
