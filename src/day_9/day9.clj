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

(def test-input2
  (clojure.string/join "\n" (concat (range 1 26) '(49 100 50))))

(defn s->i [s] ( s))


;; Solution?
(def values
  (->> (clojure.string/split (slurp "src/day_9/input.txt") #"\n")
       (mapv #(Long/valueOf %))))

(defn values->set [ls]
  (set (for [x ls y ls :when (not (= x y))] (+ x y))))

(defn calculate [all-values window-start window-end]
  (let [value (nth all-values (inc window-end))
        window (subvec all-values window-start (inc window-end))]
    (if-not (contains? (values->set window) value)
      value
      (calculate all-values (inc window-start) (inc window-end)))))

(calculate values 0 25)
;; Solution?


1212510616
