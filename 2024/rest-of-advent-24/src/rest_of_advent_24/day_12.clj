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
  (and (is-on-map? c3)
       (is-on-map? c4)
       (->> directions
            (map (partial add c4))
            (some (partial = c3)))))

(defn create-region [map c]
  (let [plant (get-plant c)]
    (if (not (map plant))
      (conj map [plant [#{c}]])
      (let [regions (map plant)
            idx (->> regions (keep-indexed (fn [idx region] (when (some (partial is-adjacent? c) region) idx)))
                             (first))]
          (if idx
              (update-in map [plant idx] (fn [region] (conj region c)))
              (update-in map [plant] (fn [regions] (conj regions #{c}))))))))

(defn vec-remove
  "remove elem in coll"
  [pos coll]
  (assert (vector? coll))
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn join-regions [regions]
  (->> regions
    (reduce (fn [acc region]
              (if (empty? acc)
                [region]
                (let [cart (fn [s1 s2] (->> s1 (mapcat (fn [a]
                                                         (->> s2 (mapcat (fn [b]
                                                                          [[a b]])))))))
                      index-of-adjacent (->> acc
                                             (keep-indexed
                                               (fn [idx r]
                                                 (when (some #(apply is-adjacent? %) (cart r region))
                                                   idx)))
                                             (first))]
                 (if index-of-adjacent
                  (conj (vec-remove index-of-adjacent acc)
                        (into (get acc index-of-adjacent) region))
                  (conj acc
                        region)))))
            [])))

(def regions
  (->> indices
       (reduce create-region {})
       (map (fn [[k v]] [k (join-regions v)]))
       (into {})))

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

(time
  (->> regions
       (seq)
       (mapcat second)
       (map #(* (calc-area %) (calc-perimeter %)))
       (reduce +)))
