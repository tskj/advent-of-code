#!/usr/bin/env bb

(require '[clojure.string :as str])

(def input
  (->> "./day-10-input-part-1.txt"
       (slurp)
       (str/split-lines)
       (map parse-long)
       sort))

(->> [[0] input [(+ 3 (last input))]]
     (apply concat)
     sort
     (partition 2 1)
     (map #(apply - %))
     (map #(* -1 %))
     frequencies
     ((fn [a] (* (a 1) (a 3)))))
