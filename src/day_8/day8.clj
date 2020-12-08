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
  (let [[_ op value] (re-find #"^(\w{3})\s([-+]{1}\d+)$" s)]
    [idx (keyword op) (Integer. value) false]))

(defn instructions [s]
  (->> (clojure.string/split s #"\n")
       (map-indexed parse)
       (vec)))

(defn run [instructions idx sum nj-counter change-nj-on]
  (let [[idx op value executed?] (get instructions idx)]
    (if (or (nil? idx) executed?)
      [(get-in instructions [(dec (count instructions)) 3]) ;; Last instruction executed?
       sum
       instructions]
      (let [op (if-not (clojure.set/subset?    ;; Change op jmp/nop
                        #{op nj-counter}
                        #{:nop :jmp change-nj-on})
                 op
                 (case op :nop :jmp :jmp :nop))]
        (run
          (assoc-in instructions [idx 3] true)
          (case op :jmp (+ idx value) (inc idx))
          (case op :acc (+ sum value) sum)
          (case op :acc nj-counter (inc nj-counter))
          change-nj-on)))))

(def p-run
  (partial
   run
   (instructions (slurp "src/day_8/input.txt"))
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
     ((comp second first)))              ;; Find the accumulated value
