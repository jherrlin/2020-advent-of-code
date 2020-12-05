(ns day-5.day5)




(defn find-row [coll begin end]
  (println "Top:" (seq coll) begin end)
  (cond
    (and (= 1 (count coll))
         (#{\B \R} (first coll)))
    (+ begin (/ (- end begin) 2))

    (and (= 1 (count coll))
         (#{\F \L} (first coll)))
    (dec (+ (/ end 2) (/ begin 2)))

    (#{\B \R} (first coll))
    (let [b (+ begin (/ (- end begin) 2))
          e end]
      (println (first coll) b e)
      (find-row (rest coll) b e))

    (#{\F \L} (first coll))
    (let [b begin
          e (+ (/ end 2) (/ begin 2))]
      (println (first coll) b e)
      (find-row (rest coll) b e))))


(defn find-it [s]
  (let [[row column] (map
                      (partial clojure.string/join "")
                      (split-at 7 s))]
    ;; (find-row row 0 128)
    (+ (* (find-row row 0 128) 8)
       (find-row column 0 8))))

(->> (clojure.string/split (slurp "src/day_5/input.txt") #"\n")
       (map find-it)
       (sort))

;; That's not the right answer; your answer is too low. If you're stuck, make
;; sure you're using the full input data; there are also some general tips on
;; the about page, or you can ask for hints on the subreddit. Please wait one
;; minute before trying again. (You guessed 122.) [Return to Day 5]


(def a (->> (clojure.string/split (slurp "src/day_5/input.txt") #"\n")
           (map find-it)
           (set)))

(def b (set (range 13 978)))

(clojure.set/difference b a)
