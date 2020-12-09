(ns day-7.day7)


;; =============== First ===============
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


(def bags
  (->> (clojure.string/split #_test-input (slurp "src/day_7/input.txt") #"\n")
       (map (fn [x]
              (let [[_ big-bag rest] (re-find #"^(\w+\s\w+)\sbags\scontain\s(.*)$" x)]
                {big-bag (->> (re-seq #"(\d)\s(\w+\s\w+)" rest)
                              (map (juxt last (comp #(Integer. %) second)))
                              (into {}))})))

       (into {})))

(defn inspect-bag-chain
  [golden-bag start]
  (loop [[this-bag & rest] [start]
         result            #{}]
    (let [inner-bags (keys (get bags this-bag))]
      (cond
        (nil? this-bag)          result
        (#{golden-bag} this-bag) (recur (into rest inner-bags) (conj result start))
        :else                    (recur (into rest inner-bags) result)))))


(->> (keys bags)
     (map (partial inspect-bag-chain "shiny gold"))
     (apply clojure.set/union)
     (remove #{"shiny gold"})
     (count))
;; =============== First ===============





;; =============== Second ===============
;; Correct answer: 8030
(def state (atom 0))

(def map*
  (->> (clojure.string/split (slurp "src/day_7/input.txt") #"\n")
       (map (fn [s]
              (if (clojure.string/includes? s "no other bags.")
                (let [id (last (re-find #"^(\w+\s\w+).*" s))]
                  {:id    id
                   :leaf  true})
                (let [[_ parent rest] (re-find #"^(\w+\s\w+)\sbags\scontain\s(.*)$" s)]
                  (merge
                   {:id parent
                    :children
                    (->> (re-seq #"(\d)\s(\w+\s\w+)" rest)
                         (map (juxt last (comp #(Integer. %) second)))
                         (into {}))})))))
       (map (juxt :id identity))
       (into {})))

(def nodes
  (clojure.walk/prewalk
   (fn [{:keys [id children] :as x}]
     (when (and (map? x) id)
       (swap! state inc))
     (if (and (map? x) children id)
       (assoc x :children
              (into {}
                    (mapcat (fn [[id count]]
                              (for [_ (range count)]
                                {(java.util.UUID/randomUUID) (get map* id)})) children)))
       x))
   (get-in map* ["shiny gold"])))

(dec @state)
;; =============== Second ===============


;; =============== Second, attempt 2 ===============
(defn bag-walk [bags counter m]
  (if-not (and (map? m) (every? number? (vals m)))
    m
    (->> m
         (mapcat
          (fn [[k v]]
            (for [_    (range v)
                  :let [_ (swap! counter inc)]]
              {(str k "-" (java.util.UUID/randomUUID))
               (get bags k)})))
         (into {}))))

(defn walk-the [bags start]
  (let [counter (atom 0)
        x       (clojure.walk/prewalk
                 (partial bag-walk bags counter)
                 start)]
    [@counter x]))

(walk-the bags (get bags "shiny gold"))
;; =============== Second, attempt 2 ===============
