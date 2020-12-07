(ns day-7.day7-2)

(def test-input
"light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")


(defn end [s]
  (let [id (last (re-find #"^(\w+\s\w+).*" s))]
    {:id    id
     :type  :end
     :count 1}))

(defn node [[s i]]
  {:id    s
   :type  :node
   :count i})

(defn parse-text-node [s]
  (if (clojure.string/includes? s "no other bags.")
    (end s)

    (let [[_ big-bag rest] (re-find #"^(\w+\s\w+)\sbags\scontain\s(.*)$" s)]
      {big-bag (->> (re-seq #"(\d)\s(\w+\s\w+)" rest)
                    (map (juxt last (comp #(Integer. %) second)))
                    (into {}))})))

(def ds (->> (clojure.string/split test-input #_(slurp "src/day_7/input.txt") #"\n")
             (map (fn [s]
                    (if (clojure.string/includes? s "no other bags.")
                      (end s)
                      (let [[_ big-bag rest] (re-find #"^(\w+\s\w+)\sbags\scontain\s(.*)$" s)]
                        {:id big-bag
                         :type :parent
                         :nodes (->> (re-seq #"(\d)\s(\w+\s\w+)" rest)
                                     (map (juxt last (comp #(Integer. %) second)))
                                     (map node)
                                     (map (juxt :id identity))
                                     (into {}))}))))
             (map (juxt :id identity))
             (into {})))



ds

(defn do-walk [ds {:keys [id type count nodes] :as current} new-ds]
  (case type
    :parent (do-walk ds )
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
