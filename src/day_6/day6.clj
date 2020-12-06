(ns day-6.day6)

;; First
(->> (clojure.string/split (slurp "src/day_6/input.txt") #"\n\n")
     (map (comp count set seq #(clojure.string/replace % #"\n" "")))
     (reduce +))

;; Second
(->> (clojure.string/split (slurp "src/day_6/input.txt") #"\n\n")
     (map #(clojure.string/split % #"\n"))
     (map (fn [x] (map (fn [y] (set (clojure.string/split y #"")))x)))
     (map (fn [x] (count (apply clojure.set/intersection x))))
     (reduce +))
