(ns day-7.day7-2)

;; Correct answer: 8030


(let [state (atom 0)
      map* (->> (clojure.string/split (slurp "src/day_7/input.txt") #"\n")
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
                (into {}))
      nodes (clojure.walk/prewalk
             (fn [{:keys [id children] :as x}]
               (if (and (map? x) children id)
                 (assoc x :children
                        (into {}
                              (mapcat (fn [[id count]]
                                        (for [_ (range count)]
                                          {(java.util.UUID/randomUUID) (get map* id)})) children)))
                 x))
             (get-in map* ["shiny gold"]))]
  (clojure.walk/prewalk
   (fn [{:keys [id] :as m}]
     (when (and (map? m) id)
       (swap! state inc))
     m)
   nodes)
  (dec @state))
