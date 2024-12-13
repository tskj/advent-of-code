(ns rest-of-advent-24.day-12
  (:require
   [clojure.string :refer [join split split-lines]]
   [rest-of-advent-24.utils.macros :refer [blk]]))

(def input
  (->> (slurp "resources/day-12-input.txt")
       (split-lines)
       (mapv vec)))

(defn get-plant [c] (get-in input c))

(def indices
  (->> input
       (map-indexed
         (fn [idy row]
           (->> row (map-indexed (fn [idx _] [idy idx])))))
       (apply concat)
       (vec)))

(def regions-by-plant
  (->> indices
       (group-by get-plant)))

(defn is-on-map? [c] ((set indices) c))

(def directions
  [[-1 0]
   [1 0]
   [0 1]
   [0 -1]])

(defn add [[a b] [c d]]
  [(+ a c)
   (+ b d)])

(defn is-adjacent? [c3 c4]
  (->> directions
       (map (partial add c4))
       (some (partial = c3))))

(defn take-first [transducer coll]
  (->> coll
       (transduce (comp transducer (take 1)) conj)
       (first)))

(defn vec-remove
  "remove elem in coll"
  [pos coll]
  (assert (vector? coll))
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn create-regions [indices-by-plant]
  (loop [input indices-by-plant
         region #{}
         regions []]
    (if (empty? input) (if (> (count region) 0) (conj regions region) regions)
        (let [adjacent-indices (->> input (filter (fn [c] (->> region (some #(is-adjacent? c %))))))]
          (if (empty? adjacent-indices)
            (recur (rest input)             ;; consume one
                   #{(first input)}         ;; start new region
                   (if (> (count region) 0) ;; put region aside
                     (conj regions region)
                     regions))
            (recur (->> input (remove (set adjacent-indices))) ;; consume all adjacent
                   (into region adjacent-indices)              ;; put in adjacent region
                   regions))))))

(def regions
  (->> regions-by-plant
       (map (fn [[k v]] v))
       (pmap create-regions)
       (apply concat)))

(defn calc-area [region] (count region))
(defn calc-perimeter [region]
  (->> region
       (map (fn [c]
              (- 4
                 (->> directions
                      (map (partial add c))
                      (keep (fn [potential] (region potential)))
                      (count)))))
       (reduce +)))

(->> regions
     (map #(* (calc-area %) (calc-perimeter %)))
     (reduce +))
