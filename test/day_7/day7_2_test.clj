(ns day-7.day7-2-test
  (:require [day-7.day7-2 :as sut]
            [clojure.test :as t]))

"shiny cyan bags contain 2 striped beige bags, 4 mirrored maroon bags, 4 dark yellow bags, 4 bright indigo bags."


(t/deftest parse-text-node
  (t/is (= (sut/parse-text-node "light chartreuse bags contain no other bags.")
           {"light chartreuse" 1}))

  (t/is (= (sut/parse-text-node "shiny cyan bags contain 2 striped beige bags, 4 mirrored maroon bags, 4 dark yellow bags, 4 bright indigo bags.")
           {"shiny cyan"
            {"striped beige" 2,
             "mirrored maroon" 4,
             "dark yellow" 4,
             "bright indigo" 4}}))

  )
