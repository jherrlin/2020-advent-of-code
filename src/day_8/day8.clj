(ns day-8.day8)


(def test-input "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")



(defn parse [s]
  (let [[_ op sign value] (re-find #"^(\w{3})\s([-+])(\d+)$" s)]
    [(keyword op) (symbol sign) (Integer. value) false 0]))

(defn run [instructions idx sum]
  (let [[op sign value executed?] (get instructions idx)]
    (if executed?
      sum
      (run
        (assoc-in instructions [idx 3] true)
        (case op :nop (inc idx) :acc (inc idx) :jmp ((eval sign) idx value))
        (case op :acc ((eval sign) sum value) sum)))))

(run
  (->> (clojure.string/split #_test-input (slurp "src/day_8/input.txt") #"\n")
       (mapv parse))
  0
  0
  )
