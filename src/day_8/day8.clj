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


(def test-input2
  "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
nop -4
acc +6")


(defn parse [idx s]
  (let [[_ op sign value] (re-find #"^(\w{3})\s([-+])(\d+)$" s)]
    [idx (keyword op) (symbol sign) (Integer. value) false 0 0]))

(defn run [instructions idx sum counter nj-counter change-nj-on]
  (let [this                              (get instructions idx)
        [idx op sign value executed? _ _] (get instructions idx)]
    (if (or (nil? idx) executed?)
      [(get-in instructions [(dec (count instructions)) 4])            ;; last instruction
       sum
       instructions]
      (let [op (if-not (and (= nj-counter change-nj-on)
                            (#{:nop :jmp} op))
                 op
                 (case op :nop :jmp :jmp :nop))]
        (run
          (-> instructions
              (assoc-in [idx 1] op)
              (assoc-in [idx 4] true)             ;; Executed
              (assoc-in [idx 5] counter)          ;; Counter
              (assoc-in [idx 6] nj-counter))      ;; nop jmp counter
          (case op :nop (inc idx) :acc (inc idx) :jmp ((eval sign) idx value))
          (case op :acc ((eval sign) sum value) sum)
          (inc counter)
          (case op :acc nj-counter (inc nj-counter))
          change-nj-on)))))

(def p-run (partial
            run
            (->> (clojure.string/split #_test-input2 (slurp "src/day_8/input.txt") #"\n") (map-indexed (fn [idx m] (parse idx m))) (vec))
            0 0 0 0))

(p-run 1)

(->> (p-run -1)
     last
     (filter (comp #{:jmp :nop} second))
     (sort-by last #(compare %2 %1))
     (count)
     (range)
     (map #(p-run %))
     (filter (comp true? first)))
