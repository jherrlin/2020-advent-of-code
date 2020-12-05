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


(println (find-row "BFFFBBF" 0 128))
(println "----------------")
(println (find-row "FFFBBBF" 0 128))
(println "----------------")
(println (find-row "BBFFBBF" 0 128))




(find-row (seq "BFFFBBF") 1 128)

[coll begin end]

(first (seq "FBFBBFF"))
(rest (seq "FBFBBFF"))


(fn b [begin end] ())

(let [begin 1
      end   128]
  [(- end (/ end 2))
   end]
  )

(defn b [start end]
  [start
   (/ end 2)])

(apply b (b 1 128))
