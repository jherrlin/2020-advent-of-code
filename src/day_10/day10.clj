(ns day-10.day10)


;; ============ First ============
(def input-values
  (->> (clojure.string/split (slurp "src/day_10/input.txt") #"\n")
       (map #(Long/Long/parseLong %))
       (sort)))

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
;; 2475
;; ============ First ============



;; ============ Second ============
(defn parse-input-values [s]
  (let [v (->> (clojure.string/split s #"\n")
               (map #(Long/Long/parseLong %))
               (sort))]
    (->> (concat (concat [0] v)
                 [(+ 3 (apply max v))])
         (sort))))

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
  ;; Solution:
  (def parsed-input (parse-input-values (slurp "src/day_10/input.txt")))
  (def adapters-map-input (adapters-map parsed-input))
  (->> parsed-input
       (segments)
       (map (partial adapters-chain adapters-map-input))
       (reduce *))

  ;; Answer: 442136281481216

  ;; The major part of this solution is to do the segmentation.
  ;; '(1 2 3    7 8 9   11)    =>
  ;; '((1 2 3) (7 8 9) (11))
  ;; The do the adapter-chain on each segment and the reduce * on them.
  )
