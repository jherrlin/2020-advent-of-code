(ns day-10.day10)


(def test-input "16
10
15
5
1
11
7
19
6
12
4
")

(def test-input2
  "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3")


;; ============ First ============
(def input-values
  (->> (clojure.string/split test-input2 #_(slurp "src/day_10/input.txt") #"\n")
       (map #(Long/Long/parseLong %))
       (sort)))

(range 0 (count input-values))

(def ds
  (loop [[this & rest] (concat (concat [0] input-values)
                               [(+ 3 (apply max input-values))])
         results       {}]
    (let [next (first rest)]
      (cond
        (nil? rest) results
        :else (recur rest (update results (- next this) (fnil inc 0)))))))


;; First
(* (get ds 3)
   (get ds 1))
;; ============ First ============



;; ============ Second ============
(defn parse-input-values [s]
  (let [v (->> (clojure.string/split s #"\n")
               (map #(Long/Long/parseLong %))
               (sort))]
    (->> (concat (concat [0] v)
                 [(+ 3 (apply max v))])
         (sort))))


(def parsed-input (parse-input-values (slurp "src/day_10/input.txt")))
(def parsed-test-input (parse-input-values test-input))
(def parsed-test-input2 (parse-input-values test-input2))

(defn adapters-map [v]
  (->> v
       (map (fn [x]
              {x (->> (for [y (range (+ 1 x) (+ 4 x))]
                        [y true])
                      (into {}))}))
       (into {})))

(defn adapters-chain
  [adapters segment]
  (loop [[this-adapter & rest] [(apply min segment)]
         arrangements          0]
    (let [inner-adapters (keys (get (select-keys adapters segment) this-adapter))]
      (cond
        (nil? this-adapter)                   arrangements
        (#{(apply max segment)} this-adapter) (recur (into rest inner-adapters) (inc arrangements))
        :else                                 (recur (into rest inner-adapters) arrangements)))))

(def adapters-map-input (adapters-map parsed-input))
(def adapters-map-test-input (adapters-map parsed-test-input))
(def adapters-map-test-input2 (adapters-map parsed-test-input2))


(defn segments
  "Split the vector up into segments.
  '(1 2 3    7 8 9   11)    =>
  '((1 2 3) (7 8 9) (11))"
  [all-values]
  (let [values (vec (sort all-values))
        at (reduce (fn [coll [idx v]]
                     (if-let [next-value (get values (inc idx))]
                       (if (= (inc v) next-value)
                         coll
                         (conj coll (nth values (inc idx))))
                       coll))
                   []
                   (map-indexed vector values))]
    (loop [lst            all-values
           [point & rest] at
           result         []]
      (if (nil? point)
        (concat result (list lst))
        (recur (drop-while (complement #{point}) lst) rest (concat result [(take-while (complement #{point}) lst)]))))))


(comment
  ;;
  (->> parsed-input
       (segments)
       (map (partial adapters-chain adapters-map-input))
       (reduce *))
  )
