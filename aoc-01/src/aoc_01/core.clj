(ns aoc-01.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def data 
  (->> "./data/input.txt"
   (slurp)
   (str/split-lines)
   (map #(str/split % #" "))
   (map (fn [[dir amount]] [dir (Integer/parseInt amount)]))))

(def example-data
  [["forward" 5]
   ["down" 5]
   ["forward" 8]
   ["up" 3]
   ["down" 8]
   ["forward" 2]])

(defn calc-result [data]
  (loop [input data
         x 0
         aim 0
         depth 0]         
    (if (empty? input)
      {:x x
       :depth depth}       
      (let [[dir amount] (first input)]
        (recur
         (rest input)       
         (if (= dir "forward") 
           (+ x amount) 
           x)

         (if (= dir "down")
           (+ aim amount)
           (if (= dir "up")
             (- aim amount)
             aim))
         
         (if (= dir "forward")
           (+ depth (* aim amount))
           depth))))))
         

(calc-result example-data)
(def res (calc-result data))
(* (res :x) (res :depth))
