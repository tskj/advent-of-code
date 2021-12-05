(ns aoc-01.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def text (slurp "./data/input.txt"))

(def list-of-str-nums
  (str/split-lines text))

(def list-of-nums
  (map #(Integer/parseInt %) list-of-str-nums))

(defn window [n list]
  (loop [input (seq list)
         output []]
    (if (< (count input) n)
      output
      (recur 
       (rest input) 
       (conj output (take n input))))))

(defn slide [list]
  (window 2 list))

(slide [])
(slide [1])
(slide [1 2])
(slide [1 2 3])
(window 3 [1 2 3 4])
(window 3 [1 2 3 4 5])
(slide list-of-nums)

(def example-data
  [199
   200
   208
   210
   200
   207
   240
   269
   260
   263])

(defn calc-result [data]
  (->> data
       (window 3)
       (map #(reduce + %))
       (slide)
       (map (fn [[x y]] (- y x)))
       (filter pos-int?)
       (count)))

(calc-result example-data)
(calc-result list-of-nums)
