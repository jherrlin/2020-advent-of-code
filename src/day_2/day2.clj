(ns user)


;; Solution 1
(reduce
 (fn [i string-to-search-in]
   (let [[_ min-count max-count c s] (re-find #"(\d+)-(\d+) (.){1}:\s(.*)$" string-to-search-in)
         min-count (Integer. min-count)
         max-count (Integer. max-count)
         count (get (frequencies (map str (seq s))) c 0)]
     [min-count max-count c s count]
     (if (and (<= min-count count)
              (>= max-count count))
       (inc i)
       i)))
 0
 (clojure.string/split (slurp "input.txt") #"\n"))



(reduce
 (fn [i string-to-search-in]
   (let [[_ min-count max-count c s]
         (re-find #"(\d+)-(\d+) (.){1}:\s(.*)$" string-to-search-in)
         count (get (frequencies (map str (seq s))) c 0)]
     (if (and (<= (Integer. min-count) count)
              (>= (Integer. max-count) count))
       (inc i)
       i)))
 0
 (clojure.string/split (slurp "input.txt") #"\n"))





;; Solution for 2

(def input (clojure.string/split (slurp "input.txt") #"\n"))


(reduce
 (fn [i string-to-search-in]
   (let [[_ min-count max-count c s] (re-find #"(\d+)-(\d+) (.){1}:\s(.*)$" string-to-search-in)
         first-pos                   (Integer. min-count)
         second-pos                  (Integer. max-count)
         first-str                   (str (nth s (inc first-pos)))
         second-str                  (str (nth s (inc second-pos)))]
     (if (= 1 (count (filter (comp #{c}) [first-str second-str])))
       (inc i)
       i
       )
     )
   )
 0
 input
 )






"j" "j"
(str (nth "jwjjjjkhjjjltjmjjjr" 8))
