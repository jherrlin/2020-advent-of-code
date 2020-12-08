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
    [idx (keyword op) (symbol sign) (Integer. value) false 0]))

(defn run [instructions idx sum nj-counter change-nj-on]
  (let [[idx op sign value executed? _] (get instructions idx)]
    (if (or (nil? idx) executed?)
      [(get-in instructions [(dec (count instructions)) 4])
       sum
       instructions]
      (let [op (if-not (and (= nj-counter change-nj-on)
                            (#{:nop :jmp} op))
                 op
                 (case op :nop :jmp :jmp :nop))]
        (run
          (-> instructions                        ;; Update instruction
              (assoc-in [idx 1] op)               ;; Set op as it may have changed
              (assoc-in [idx 4] true))            ;; Executed?
          (case op :nop (inc idx) :acc (inc idx) :jmp ((eval sign) idx value))
          (case op :acc ((eval sign) sum value) sum)
          (case op :acc nj-counter (inc nj-counter))
          change-nj-on)))))

(def p-run
  (partial
   run
   (->> (clojure.string/split (slurp "src/day_8/input.txt") #"\n")
        (map-indexed (fn [idx m] (parse idx m))) (vec))
   0 0 0))

;; First problem
(second (p-run -1))

;; Second Problem
(->> (p-run -1)                          ;; Run without op manipulation
     last                                ;; Get the instruction vector
     (filter (comp #{:jmp :nop} second)) ;; Filter the jmp and nop instructins
     (count)                             ;; Count the number of jmp and nop instructions
     (range)                             ;; Create a range from the number of jmp and nop instr
     (map #(p-run %))                    ;; Run with op manipulation on range number N
     (filter (comp true? first))         ;; Filter the result that finished to last instruction
     (first)                             ;; Take the first result
     (second))                           ;; Find the accumulated value
