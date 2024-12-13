(ns rest-of-advent-24.day-13
  (:require
   [clojure.string :refer [split-lines]]))

(def input
  (->> (slurp "resources/day-13-input.txt")
       (split-lines)
       (mapv vec)))

