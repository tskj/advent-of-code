(ns rest-of-advent-24.utils.elves)

(defn includes [x coll]
  (some #(= % x) coll))
