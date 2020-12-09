(ns day-9.day9)

(def test-input "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")




(def values
  (->> (clojure.string/split #_test-input (slurp "src/day_9/input.txt") #"\n")
       (mapv #(Long/valueOf %))))

(defn calc-window-values [window-values]
  (set (for [x window-values
             y window-values
             :when (not (= x y))]
         (+ x y))))

(defn calculate [all-values window-start window-end]
  (let [value (nth all-values (inc window-end))
        window (subvec all-values window-start (inc window-end))]
    (if-not (contains? (calc-window-values window) value)
      value
      (calculate all-values (inc window-start) (inc window-end)))))

;; First
(def first-finding
  (calculate values 0 25))

(defn find-a-contiguous-set [all-values value-to-find window-start window-end window-size]
  (when (< window-end (count all-values))
    (let [window (subvec all-values window-start window-end)]
      (if (#{value-to-find} (reduce + window))
        (->> window ((juxt (partial apply min) (partial apply max))) (apply +))
        (find-a-contiguous-set all-values value-to-find (inc window-start) (inc window-end) window-size)))))

(map (fn [window-size]
       (find-a-contiguous-set
        values
        first-finding
        0
        window-size
        window-size))
     (range 2 (count values)))
