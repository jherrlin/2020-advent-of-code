(ns day-6.day6)

;; First
(->> (clojure.string/split (slurp "src/day_6/input.txt") #"\n\n")
     (map (comp count set seq #(clojure.string/replace % #"\n" "")))
     (reduce +))

;; Second
(use '[clojure.string :only (split)])
(->> (split (slurp "src/day_6/input.txt") #"\n\n")
     (map #(for [s (split % #"\n")] (set (split s #""))))
     (map #(count (apply clojure.set/intersection %)))
     (reduce +))


;; Second with a transducer
(transduce
 (comp
  (map #(for [s (split % #"\n")] (set (split s #""))))
  (map #(count (apply clojure.set/intersection %))))
 +
 (split (slurp "src/day_6/input.txt") #"\n\n"))
