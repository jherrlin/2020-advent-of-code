(ns day-5.day5)




(defn find-row [coll begin end]
  (println "Top:" (seq coll) begin end)
  (cond
    (and (= 1 (count coll))
         (= \B (first coll)))
    (dec (+ begin (/ (- end begin) 2)))

    (and (= 1 (count coll))
         (= \F (first coll)))
    (dec (+ (/ end 2) (/ begin 2)))

    (= \B (first coll))
    (let [b (+ begin (/ (- end begin) 2))
          e end]
      (println (first coll) b e)
      (find-row (rest coll) b e))

    (= \F (first coll))
    (let [b begin
          e (+ (/ end 2) (/ begin 2))]
      (println (first coll) b e)
      (find-row (rest coll) b e))))
