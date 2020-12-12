(ns day-11.day11-2)


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

(defn next-state [current look]
  (cond
    (and (#{"L"} current) (= 8 (count (filter #{:free}     look)))) "#"
    (and (#{"#"} current) (< 4 (count (filter #{:occupied} look)))) "L"
    :else                                                           current))

(defn next-w  [[x y] gen] (when (get-in gen [     y  (dec x)]) [(dec x)      y ]))
(defn next-nw [[x y] gen] (when (get-in gen [(dec y) (dec x)]) [(dec x) (dec y)]))
(defn next-n  [[x y] gen] (when (get-in gen [(dec y)      x ]) [     x  (dec y)]))
(defn next-ne [[x y] gen] (when (get-in gen [(dec y) (inc x)]) [(inc x) (dec y)]))
(defn next-e  [[x y] gen] (when (get-in gen [     y  (inc x)]) [(inc x)      y ]))
(defn next-se [[x y] gen] (when (get-in gen [(inc y) (inc x)]) [(inc x) (inc y)]))
(defn next-s  [[x y] gen] (when (get-in gen [(inc y)      x])  [     x  (inc y)]))
(defn next-sw [[x y] gen] (when (get-in gen [(inc y) (dec x)]) [(dec x) (inc y)]))

(defn look [f [x y] generation]
  (loop [next-x-y [x y]]
    (let [next-x-y-value (individual-state (f next-x-y generation) generation)]
      (cond
        (nil? next-x-y-value) :free
        (#{"."} next-x-y-value) (recur (f next-x-y generation))
        (#{"#"} next-x-y-value) :occupied
        (#{"L"} next-x-y-value) :free))))

(def look-map
  {:look-w  (partial look next-w)
   :look-nw (partial look next-nw)
   :look-n  (partial look next-n)
   :look-ne (partial look next-ne)
   :look-e  (partial look next-e)
   :look-se (partial look next-se)
   :look-s  (partial look next-s)
   :look-sw (partial look next-sw)})

(defn look-around [x-y generation]
  (map
   (fn [k]
     ((get look-map k) x-y generation))
   (keys look-map)))

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
            (next-state
             (individual-state [x y] old-gen)
             (look-around [x y] old-gen))))

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

;; Problem 2
(comment
  (print
   (->> (slurp "src/day_11/input.txt")
        (parse-input)
        (stabilized)
        (filter #{\#})
        (count)))
  )
