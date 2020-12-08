(ns day-7.day7)


;; Correct answer is 300


(def text
  "dull gold bags contain 1 light tomato bag, 4 striped lavender bags, 2 shiny tomato bags, 2 plaid lime bags.
posh lavender bags contain 5 light magenta bags, 1 dim lime bag, 1 vibrant red bag, 1 light purple bag.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
light blue bags contain 4 shiny gold bags, 5 dotted beige bags, 1 dim brown bag, 2 shiny fuchsia bags.
faded blue bags contain no other bags.")

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



(defn keys-in
  "Returns a sequence of all key paths in a given map using DFS walk."
  [m]
  (letfn [(children [node]
            (let [v (get-in m node)]
              (if (map? v)
                (map (fn [x] (conj node x)) (keys v))
                [])))
          (branch? [node] (-> (children node) seq boolean))]
    (->> (keys m)
         (map vector)
         (mapcat #(tree-seq branch? children %)))))


(count (clojure.string/split (slurp "src/day_7/input.txt") #"\n"))

;; direct
#{"vibrant indigo" "light blue" "vibrant plum" "vibrant orange" "dull orange"
  "shiny tan"}

(let [paths (->> (clojure.string/split #_test-input (slurp "src/day_7/input.txt") #"\n")
                 (remove #(clojure.string/includes? % "no other bags"))
                 (map (fn [x]
                        (let [[_ big-bag rest] (re-find #"^(\w+\s\w+)\sbags\scontain\s(.*)$" x)]
                          {big-bag (->> (re-seq #"(\d)\s(\w+\s\w+)" rest)
                                        (map (juxt last (comp #(Integer. %) second)))
                                        (into {}))})))

                 (into {})
                 ;; clojure.walk/postwalk-demo
                 ;; clojure.walk/prewalk-demo
                 keys-in

                 ;; (into #{})
                 )
      direct (->> paths
                  (remove (comp #{"shiny gold"} first ))
                  (map set)
                  (filter #(contains? % "shiny gold"))
                  (reduce (fn [s x] (into s x)) #{})
                  (remove #{"shiny gold"})
                  (set)
                  )
      direct #{"plaid gold" "plaid purple" "drab black" "clear violet" "wavy plum"
  "wavy indigo" "drab blue" "dotted brown" "dark gold" "dull maroon"
  "pale turquoise" "bright violet" "mirrored cyan" "muted yellow"
  "vibrant olive" "shiny purple" "dim aqua" "dark beige" "muted lime"
  "faded lime" "light red" "dim gray" "mirrored magenta" "dull bronze"
  "posh purple" "clear green" "drab chartreuse" "shiny lime" "vibrant beige"
  "dim lime" "vibrant indigo" "posh plum" "dull chartreuse" "vibrant cyan"
  "dotted orange" "dim crimson" "clear coral" "shiny red" "bright chartreuse"
  "dotted fuchsia" "dotted green" "dull violet" "clear tomato"
  "striped fuchsia" "light fuchsia" "clear plum" "faded yellow"
  "mirrored bronze" "plaid red" "plaid coral" "faded violet" "faded turquoise"
  "shiny indigo" "wavy green" "wavy coral" "vibrant green" "light cyan"
  "muted beige" "clear olive" "vibrant maroon" "dull gold" "dim indigo"
  "posh gold" "dotted black" "mirrored tomato" "striped magenta" "dark fuchsia"
  "drab bronze" "dull black" "bright coral" "muted salmon" "dark tomato"
  "light beige" "dull lavender" "dark aqua" "drab cyan" "vibrant purple"
  "shiny cyan" "pale lavender" "striped violet" "faded coral" "shiny orange"
  "drab aqua" "light silver" "wavy lavender" "striped red" "light turquoise"
  "dark yellow" "clear black" "clear turquoise" "faded fuchsia"
  "mirrored black" "posh lime" "shiny teal" "dotted white" "bright white"
  "dim black" "bright gold" "dull red" "vibrant fuchsia" "posh aqua"
  "posh salmon" "drab salmon" "vibrant blue" "dim salmon" "mirrored silver"
  "drab orange" "drab teal" "dull aqua" "dotted turquoise" "wavy tan"
  "dim chartreuse" "dotted magenta" "light black" "shiny olive" "faded brown"
  "dark indigo" "light olive" "muted green" "drab plum" "clear crimson"
  "pale fuchsia" "striped maroon" "wavy gray" "pale gold" "mirrored gray"
  "bright plum" "mirrored turquoise" "pale plum" "muted gold"
  "mirrored lavender" "bright lime" "dim maroon" "wavy purple" "striped bronze"
  "vibrant red" "dotted coral" "dull indigo" "wavy lime" "striped plum"
  "dotted silver" "dotted tomato" "mirrored aqua" "dark salmon"
  "vibrant bronze" "wavy olive" "dull yellow" "vibrant gold" "faded green"
  "dark blue" "faded plum" "light bronze" "striped crimson" "dotted purple"
  "dim lavender" "light tomato" "muted cyan" "faded chartreuse" "shiny brown"
  "dotted indigo" "posh fuchsia" "drab coral" "light tan" "faded aqua"
  "pale magenta" "plaid plum" "dim plum" "wavy bronze" "light blue"
  "mirrored red" "faded indigo" "wavy black" "mirrored salmon" "striped lime"
  "plaid beige" "plaid silver" "posh coral" "posh beige" "muted silver"
  "muted turquoise" "dark silver" "pale white" "bright gray" "muted orange"
  "clear orange" "bright purple" "dark crimson" "dark red" "plaid salmon"
  "muted crimson" "faded magenta" "posh violet" "striped chartreuse"
  "dim brown" "muted tan" "light purple" "bright blue" "posh crimson"
  "faded red" "dotted lavender" "muted violet" "light aqua" "striped blue"
  "clear salmon" "vibrant magenta" "muted fuchsia" "drab gray"
  "shiny chartreuse" "clear lime" "posh lavender" "dotted crimson"
  "light indigo" "vibrant plum" "drab silver" "wavy aqua" "wavy teal"
  "plaid bronze" "vibrant brown" "shiny aqua" "vibrant orange" "light magenta"
  "striped purple" "dark lime" "vibrant black" "striped silver" "muted plum"
  "muted lavender" "dotted gold" "bright green" "dim turquoise" "clear bronze"
  "shiny bronze" "light salmon" "light violet" "mirrored green" "muted red"
  "posh bronze" "faded maroon" "dotted cyan" "muted indigo" "plaid magenta"
  "plaid lime" "muted white" "vibrant aqua" "dim magenta" "dim orange"
  "posh yellow" "dull beige" "drab magenta" "muted black" "mirrored crimson"
  "bright maroon" "striped indigo" "wavy brown" "striped orange" "dull orange"
  "dull silver" "dull magenta" "mirrored maroon" "wavy tomato" "clear purple"
  "dotted lime" "mirrored chartreuse" "clear beige" "mirrored violet"
  "posh green" "light maroon" "posh tomato" "drab gold" "light white"
  "wavy yellow" "muted maroon" "dull olive" "dotted olive" "vibrant gray"
  "faded black" "muted magenta" "faded beige" "clear aqua" "dotted yellow"
  "pale tan" "posh chartreuse" "drab tomato" "dark maroon" "dark coral"
  "clear blue" "shiny tan" "striped aqua" "pale purple" "shiny maroon"
  "vibrant tomato" "bright beige" "bright brown" "dim white" "striped teal"}
      ]
  ;; paths
  (->> paths
       (filter (comp direct last))
       (map first)
       (set)
       (into direct)
       (count)
       )
  ;; direct

  )

;; 584
;; 584

(let [a ]
  (+ (->> a
          (map (comp (partial into {}) vals))
          (filter #(and (contains? % "shiny gold")
                        (<= 1 (get % "shiny gold"))
                        ))
          #_(reduce (fn [sum x] (+ sum (get x "shiny gold"))) 0))
     #_(->> a
          (filter (comp #{"shiny gold"} ffirst))
          (count)
          ))
  #_(count a)
  )

(ffirst {"vibrant orange" {"dim cyan" 3, "shiny gold" 1, "shiny salmon" 1}})

(map
 (comp #(contains? % "shiny gold") (partial into {}) vals)
 [{"vibrant orange" {"dim cyan" 3, "shiny gold" 1, "shiny salmon" 1}}]
 )

(->> '({"vibrant orange" {"dim cyan" 3, "shiny gold" 1, "shiny salmon" 1}}
       {"dull orange" {"striped beige" 2, "posh cyan" 2, "shiny gold" 2}}
       {"shiny tan" {"shiny gold" 1}}
       {"vibrant indigo"
        {"dim violet" 3, "pale yellow" 3, "pale indigo" 1, "shiny gold" 5}}
       {"vibrant plum" {"dull turquoise" 3, "shiny gold" 3, "dark black" 5}}
       {"shiny gold"
        {"shiny gold" 4, "dotted beige" 5, "dim brown" 1, "shiny fuchsia" 2}})
     (filter (comp #(contains? % "shiny gold") set keys))
     )

()

(keys {"vibrant orange" {"dim cyan" 3, "shiny gold" 1, "shiny salmon" 1}})

(into {} (vals
         {"dull gold"
          {"light tomato" 1, "striped lavender" 4, "shiny tomato" 2, "plaid lime" 2}}))


Integer/parseInt

(Integer. "1")

(->> (re-seq #"(\d)\s(\w+\s\w+)" "1 light tomato bag, 4 striped lavender bags, 2 shiny tomato bags, 2 plaid lime bags.")
     (map (juxt last second))
     )

(let [[_ n color] ]
  )

(->>
     ;; flatten
     ;; set
     )


(re-find #"^([a-z]+ [a-z]+) bags ((contain no other)|((\d) ([a-z]+ [a-z]+)))" "faded blue bags contain no other bags.")


(re-find #"^([a-z]+ [a-z]+) bags[, .]+((contain no other)?|((\d) ([a-z]+ [a-z]+)))" "posh lavender bags contain 5 light magenta bags, 1 dim lime bag, 1 vibrant red bag, 1 light purple bag.")



(re-find #"^(\w+\s\w+)\sbags.*$" "posh lavender bags contain 5 light magenta bags, 1 dim lime bag, 1 vibrant red bag, 1 light purple bag.")

(re-find #"(\s?(\d) (\w+\s\w+) bags])" "5 light magenta bags")

(re-find #"(\s?(\d) (\w+\s\w+) bags]).*" " 5 light magenta bags, 1 dim lime bag, 1 vibrant red bag, 1 light purple bag.")


"dull gold bags contain 1 light tomato bag, 4 striped lavender bags, 2 shiny tomato bags, 2 plaid lime bags."

(re-find #"^(\w+\s\w+)\sbags\scontain(.*)$" "posh lavender bags contain 5 light magenta bags, 1 dim lime bag, 1 vibrant red bag, 1 light purple bag.")






(let [a (->> (clojure.string/split (slurp "src/day_7/input.txt") #"\n")
            (remove #(clojure.string/includes? % "no other bags"))
            (map (fn [x]
                   (let [[_ big-bag rest] (re-find #"^(\w+\s\w+)\sbags\scontain\s(.*)$" x)]
                     {big-bag (->> (re-seq #"(\d)\s(\w+\s\w+)" rest)
                                   (map (juxt last (comp #(Integer. %) second)))
                                   (into {}))})))
            (filter (comp #(contains? % "shiny gold") (partial into {}) vals) x)


            ;; (map (comp #(contains? % "shiny gold") (partial into {}) vals))
            ;; (map ffirst)
            ;; (set)
            ;; (count)
            )]
  (+ (->> a
          (map (comp (partial into {}) vals))
          (filter #(and (contains? % "shiny gold")

                        ))
          (reduce (fn [sum x] (+ sum (get x "shiny gold"))) 0))
     #_(->> a
          (filter (comp #{"shiny gold"} ffirst))
          (count)
          ))
  (count a)
  a
  )

(ffirst {"vibrant orange" {"dim cyan" 3, "shiny gold" 1, "shiny salmon" 1}})

(map
 (comp #(contains? % "shiny gold") (partial into {}) vals)
 [{"vibrant orange" {"dim cyan" 3, "shiny gold" 1, "shiny salmon" 1}}]
 )

(->> '({"vibrant orange" {"dim cyan" 3, "shiny gold" 1, "shiny salmon" 1}}
       {"dull orange" {"striped beige" 2, "posh cyan" 2, "shiny gold" 2}}
       {"shiny tan" {"shiny gold" 1}}
       {"vibrant indigo"
        {"dim violet" 3, "pale yellow" 3, "pale indigo" 1, "shiny gold" 5}}
       {"vibrant plum" {"dull turquoise" 3, "shiny gold" 3, "dark black" 5}}
       {"shiny gold"
        {"shiny gold" 4, "dotted beige" 5, "dim brown" 1, "shiny fuchsia" 2}})
     (filter (comp #(contains? % "shiny gold") set keys))
     )

()

(keys {"vibrant orange" {"dim cyan" 3, "shiny gold" 1, "shiny salmon" 1}})

(into {} (vals
         {"dull gold"
          {"light tomato" 1, "striped lavender" 4, "shiny tomato" 2, "plaid lime" 2}}))


Integer/parseInt

(Integer. "1")

(->> (re-seq #"(\d)\s(\w+\s\w+)" "1 light tomato bag, 4 striped lavender bags, 2 shiny tomato bags, 2 plaid lime bags.")
     (map (juxt last second))
     )

(let [[_ n color] ]
  )

(->>
     ;; flatten
     ;; set
     )


(re-find #"^([a-z]+ [a-z]+) bags ((contain no other)|((\d) ([a-z]+ [a-z]+)))" "faded blue bags contain no other bags.")


(re-find #"^([a-z]+ [a-z]+) bags[, .]+((contain no other)?|((\d) ([a-z]+ [a-z]+)))" "posh lavender bags contain 5 light magenta bags, 1 dim lime bag, 1 vibrant red bag, 1 light purple bag.")



(re-find #"^(\w+\s\w+)\sbags.*$" "posh lavender bags contain 5 light magenta bags, 1 dim lime bag, 1 vibrant red bag, 1 light purple bag.")

(re-find #"(\s?(\d) (\w+\s\w+) bags])" "5 light magenta bags")

(re-find #"(\s?(\d) (\w+\s\w+) bags]).*" " 5 light magenta bags, 1 dim lime bag, 1 vibrant red bag, 1 light purple bag.")


"dull gold bags contain 1 light tomato bag, 4 striped lavender bags, 2 shiny tomato bags, 2 plaid lime bags."

(re-find #"^(\w+\s\w+)\sbags\scontain(.*)$" "posh lavender bags contain 5 light magenta bags, 1 dim lime bag, 1 vibrant red bag, 1 light purple bag.")
