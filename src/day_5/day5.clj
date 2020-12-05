(ns day-5.day5)

(defn find* [coll begin end]
  (cond
    (and (= 1 (count coll)) (#{\B \R} (first coll)))
    (+ begin (/ (- end begin) 2))
    (and (= 1 (count coll)) (#{\F \L} (first coll)))
    (dec (+ (/ end 2) (/ begin 2)))
    (#{\B \R} (first coll))
    (find* (rest coll) (+ begin (/ (- end begin) 2)) end)
    (#{\F \L} (first coll))
    (find* (rest coll) begin (+ (/ end 2) (/ begin 2)))))

(defn find-it [s]
  (let [[row column] (map
                      (partial clojure.string/join "")
                      (split-at 7 s))]
    (+ (* (find* row 0 128) 8)
       (find* column 0 8))))

(def seats
  (map find-it
       (clojure.string/split (slurp "src/day_5/input.txt") #"\n")))

;; First
(-> seats sort last)

;; Second
(let [sorted (sort seats)]
  (clojure.set/difference
   (set (range (first sorted) (last sorted)))
   (set seats)))
