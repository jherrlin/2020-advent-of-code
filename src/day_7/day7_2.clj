(ns day-7.day7-2)

(def test-input-1
"light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")


(def test-input-2
  "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.")


(defn leaf [s]
  (let [id (last (re-find #"^(\w+\s\w+).*" s))]
    {:id    id
     :leaf  true}))

(defn node [[s i]]
  {:id    s
   :count i})

(def ds (->> (clojure.string/split #_test-input-2 (slurp "src/day_7/input.txt") #"\n")
             (map (fn [s]
                    (if (clojure.string/includes? s "no other bags.")
                      (leaf s)
                      (let [[_ parent rest] (re-find #"^(\w+\s\w+)\sbags\scontain\s(.*)$" s)]
                        (merge
                         {:id parent
                          :children
                          (->> (re-seq #"(\d)\s(\w+\s\w+)" rest)
                               (map (juxt last (comp #(Integer. %) second)))
                               #_(map node)
                               #_(map (juxt :id identity))
                               (into {}))})))))
             (map (juxt :id identity))
             (into {})))


ds




(def abc (clojure.walk/prewalk
          (fn [{:keys [id children] :as x}]
            (println x)
            (if (and (map? x) children id)
              (assoc x :children  (into {} (mapcat (fn [[id count]]
                                                     (for [_ (range count)]
                                                       {(java.util.UUID/randomUUID) (get ds id)})) children)))
              x))
          (get-in ds ["shiny gold"])))

{:id "vibrant plum", :children {"faded blue" 5,
                                "dotted black" 6}}

(def state (atom 0))
(dec @state)
(swap! state inc)

(clojure.walk/prewalk
 (fn [{:keys [id] :as m}]
   (when (and (map? m) id)
     (swap! state inc)
     )
   m)
 abc
 )


(clojure.walk/prewalk-demo
 (get-in ds ["shiny gold"]))

(-> {"dark red" 2} last last)
(-> ["dark red" 2] last)


(clojure.walk/prewalk
 (fn [{:keys [id count] :as x}]
   (if (list? x)
     (map (fn [{:keys [id count]}]
            (for [_ (range count)] (get ds id))))
     x))
 (get-in ds ["shiny gold"]))



(defn do-walk* [paths {:keys [id children leaf] :as node}]
  (if leaf
    (conj paths id)
    (do-walk* (conj paths id) (doseq [c children]))
    )
  )

(let [{:keys [children]} (get-in ds ["shiny gold"])]
  children
  )


(defn do-walk [ds {:keys [id children type count] :as current}]
  (cond
    (= type :end)  1
    (seq children) (reduce (fn [sum {:keys [id count]}] (* )) 0 children)
    :node   (do-walk ds )
    new-ds
    )
  )


(do-walk ds (get-in ds ["shiny gold"]) {})



(->> (clojure.string/split test-input #_(slurp "src/day_7/input.txt") #"\n")
     (map parse-text-node)
     (into {})
     (clojure.walk/postwalk-demo)
     )

(let [paths ]
  ;; direct

  )
