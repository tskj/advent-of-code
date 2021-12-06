(ns aoc-01.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def data
  (->> "./data/input.txt"
       (slurp)
       (str/split-lines)))

(def example-data
  ["00100"
   "11110"
   "10110"
   "10111"
   "10101"
   "01111"
   "00111"
   "11100"
   "10000"
   "11001"
   "00010"
   "01010"])

(defn to-binary [list-of-strs]
  (->> list-of-strs
       (map #(str/split % #""))
       (map #(map (fn [i] (Integer/parseInt i)) %))))

(def example-binary (to-binary example-data))
(def binary (to-binary data))

(defn to-decimal [binary]
  (loop [bin (reverse binary)
         power 0
         res 0]
    (if (empty? bin)
      res
      (recur
       (rest bin)
       (+ power 1)
       (+ res (* (first bin) (Math/pow 2 power)))))))

(def example-dec (map to-decimal example-binary))

(defn transpose [list-of-lists]
  (loop [old-matrix list-of-lists
         new-matrix []]
    (if (every? empty? old-matrix)
      new-matrix
      (recur
        (map rest old-matrix)       
        (conj new-matrix (map first old-matrix))))))

(defn gamma-rate [input]
  (let [number-of-1-bits (->> input
                              (transpose)
                              (map #(filter odd? %))
                              (map count))]
    (map
     #(if (> (* 2 %) (count input)) 1 0)
     number-of-1-bits)))

(defn flip-bits [binary]
  (map 
   #(if (= 0 %) 1 0)
   binary))

(defn epsilon-rate-from-gamma [gamma]
  (flip-bits gamma))

(defn calc [binary]
  (let [gamma-rate (gamma-rate binary)
        epsilon-rate (epsilon-rate-from-gamma gamma-rate)]
    (* (to-decimal gamma-rate)
       (to-decimal epsilon-rate))))

(calc example-binary)
(calc binary)