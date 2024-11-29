(ns aoc-01.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def input-file "./data/example.txt")
;; (def input-file "./data/input.txt")

(def initial-fish
  (->> input-file
       (slurp)
       (#(str/split % #","))
       (map #(Integer/parseInt %))))

(defn update-fish [fish]
  (if (<= fish 0)
    [6 8]
    [(dec fish)]))

(def days-to-simulate 256)

(def fishies 
  (loop [fish initial-fish
         day 0]
    ;; (print "After " day " days: " fish "\n")
    (if (<= days-to-simulate day)
      fish
      (recur
       (mapcat update-fish fish)
       (+ 1 day)))))

(count fishies)