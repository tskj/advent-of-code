(ns aoc-01.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def input-file "./data/example.txt")
;; (def input-file "./data/input.txt")

(def initial-fish
  (->> "2,1,1,4,4,1,3,4,2,4,2,1,1,4,3,5,1,1,5,1,1,5,4,5,4,1,5,1,3,1,4,2,3,2,1,2,5,5,2,3,1,2,3,3,1,4,3,1,1,1,1,5,2,1,1,1,5,3,3,2,1,4,1,1,1,3,1,1,5,5,1,4,4,4,4,5,1,5,1,1,5,5,2,2,5,4,1,5,4,1,4,1,1,1,1,5,3,2,4,1,1,1,4,4,1,2,1,1,5,2,1,1,1,4,4,4,4,3,3,1,1,5,1,5,2,1,4,1,2,4,4,4,4,2,2,2,4,4,4,2,1,5,5,2,1,1,1,4,4,1,4,2,3,3,3,3,3,5,4,1,5,1,4,5,5,1,1,1,4,1,2,4,4,1,2,3,3,3,3,5,1,4,2,5,5,2,1,1,1,1,3,3,1,1,2,3,2,5,4,2,1,1,2,2,2,1,3,1,5,4,1,1,5,3,3,2,2,3,1,1,1,1,2,4,2,2,5,1,2,4,2,1,1,3,2,5,5,3,1,3,3,1,4,1,1,5,5,1,5,4,1,1,1,1,2,3,3,1,2,3,1,5,1,3,1,1,3,1,1,1,1,1,1,5,1,1,5,5,2,1,1,5,2,4,5,5,1,1,5,1,5,5,1,1,3,3,1,1,3,1"
       (#(str/split % #","))
       (map #(Integer/parseInt %))))

(defn update-fish [[fish count]]
  (assert (not (neg? fish)))
  (if (= fish 0)
    [[6 count] [8 count]]
    [[(dec fish) count]]))

(def days-to-simulate 256)

(->>
  (group-by first [[6 3] [8 3] [0 1] [8 1]])
  (map (fn [[k v]] [k (reduce + (map second v))])))

(def fishies
  (loop [counts (frequencies initial-fish)
         day 0]
    (if (<= days-to-simulate day)
      counts
      (let [updated-fishies (->> (mapcat update-fish counts)
                                 (group-by first)
                                 (map (fn [[k v]] [k (reduce + (map second v))]))
                                 (into {}))]
          (recur
           updated-fishies
           (inc day))))))

(reduce + (map second fishies))
