(ns rest-of-advent-24.day-09
  (:require
   [clojure.string :refer [split-lines]]
   [rest-of-advent-24.utils.macros :refer [blk]]))

(def input
  (->> (slurp "resources/day-09-input.txt")
       (split-lines)
       (first)
       (vec)
       (mapv (comp parse-long str))))

(def x
  (reduce (fn [[toggle idx acc] x]
            (if (= toggle :block)
                [:free (inc idx) (into acc (repeat x idx))]
                [:block idx (into acc (repeat x \.))]))
          [:block 0 []] input))

(def free-spaces
  (blk
    (const [toggle idx acc] x)
    (->> acc
      (keep-indexed (fn [idx x]
                       (when (= x \.) idx))))))

(defn is-compacted? [output]
  (loop [output output
         is-eof? false]
   (blk
    (if (empty? output) (return true))
    (const x (first output))
    (if (and is-eof? (not= x \.)) (return false))
    (if (= x \.) (return (recur (rest output) true)))
    (assert (= is-eof? false))
    (recur (rest output) is-eof?))))

(def file
  (loop [input (keep (fn [x] (when (not= \. x) x)) (first (drop 2 x)))
         free-spaces free-spaces
         output (first (drop 2 x))]
      (let [index-of-last-digit (last (keep-indexed (fn [idx x] (when (not= \. x) idx)) output))]
        (if (is-compacted? output)
          output
          (recur (butlast input)
                 (rest free-spaces)
                 (-> (update-in output [(first free-spaces)] (fn [_] (last input)))
                     (update-in [index-of-last-digit] (fn [_] \.))))))))

file

(->> file
     (filter #(not= \. %))
     (map-indexed (fn [idx x] (* idx x)))
     (reduce +))
