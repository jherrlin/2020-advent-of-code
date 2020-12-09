(ns day-9.day9)


(def input-values
  (->> (clojure.string/split (slurp "src/day_9/input.txt") #"\n")
       (mapv #(Long/valueOf %))))

(defn calc-window-values [window-values]
  (set (for [x window-values
             y window-values
             :when (not (= x y))]
         (+ x y))))

(defn fist-number [input-values window-start window-end]
  (let [value  (nth input-values (inc window-end))
        window (subvec input-values window-start (inc window-end))]
    (if-not ((calc-window-values window) value)
      value
      (fist-number input-values (inc window-start) (inc window-end)))))

;; First
(def first-finding
  (fist-number input-values 0 25))

(defn find-a-contiguous-set [input-values value-to-find window-start window-end window-size]
  (when (< window-end (count input-values))
    (let [window (subvec input-values window-start window-end)]
      (if (#{value-to-find} (reduce + window))
        (->> window ((juxt (partial apply min) (partial apply max))) (apply +))
        (find-a-contiguous-set input-values value-to-find (inc window-start) (inc window-end) window-size)))))

;; Second
(->> (range 2 (count input-values))
     (map #(find-a-contiguous-set input-values first-finding 0 % %))
     (remove nil?)
     (first))
