(ns day-9.day9)



(def values
  (->> (clojure.string/split (slurp "src/day_9/input.txt") #"\n")
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

(calculate values 0 25)
