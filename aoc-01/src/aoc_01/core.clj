(ns aoc-01.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def text (slurp "./data/input.txt"))

(def list-of-str-nums
  (str/split-lines text))

(def list-of-nums
  (map #(Integer/parseInt %) list-of-str-nums))

(defn slide [list]
  (if (< (count list) 2)
    '()
    (let [[x-1 x-2 & xs] list
          rest (conj xs x-2)]
      (conj
       (slide rest)
       [x-1 x-2])))) 

(slide [1 2 3])
(slide [1 2 3 4])
(slide [])
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
  (->> (slide data)  
    (map (fn [[x y]] (- y x)))
    (filter pos-int?)
    (count)))

(calc-result example-data)
(calc-result list-of-nums)
