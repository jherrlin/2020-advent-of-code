(ns user)

(def inputs (map #(Integer. %) (clojure.string/split (slurp "input.txt") #"\n")))
(for [x inputs
      y inputs
      :when (= (+ x y) 2020)]
  (* x y))


(for [x (map #(Integer. %) (clojure.string/split (slurp "input.txt") #"\n"))
      y (map #(Integer. %) (clojure.string/split (slurp "input.txt") #"\n"))
      z (map #(Integer. %) (clojure.string/split (slurp "input.txt") #"\n"))
      :when (= (+ x y z) 2020)]
  (* x y z))
