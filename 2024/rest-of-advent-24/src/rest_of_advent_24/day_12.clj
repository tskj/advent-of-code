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
       (into #{})))

(defn is-on-map? [c] (indices c))

(def directions
  [[-1 0]
   [1 0]
   [0 1]
   [0 -1]])

(defn add [[a b] [c d]]
  [(+ a c)
   (+ b d)])

(defn create-region [c]
  (let [plant (get-plant c)]
    (loop [current-region #{c}]

      (let [new-plant-coords (->> directions
                                  (mapcat (fn [dir] (->> current-region (map #(add % dir)))))
                                  (filter is-on-map?)
                                  (filter (fn [new-c] (= plant (get-plant new-c)))))
            new-region (into current-region new-plant-coords)]

        (if (= current-region new-region)
            current-region
            (recur new-region))))))

(def regions
  (->> indices
       (map create-region)
       (into #{})))

(->> regions
     (group-by (comp get-plant first)))

(defn edges [c]
  [#{c              (add c [0 1])}
   #{c              (add c [1 0])}
   #{(add c [0 1])  (add c [1 1])}
   #{(add c [1 0])  (add c [1 1])}])

(defn join-edge [e1 e2]
  (let [all-corners (concat e1 e2)]
    (->> all-corners
        (filter (fn [corner] (= 1 (count (filter #(= % corner) all-corners)))))
        (into #{}))))

(defn share-corner? [e1 e2]
  (let [all-corners (into #{} (concat e1 e2))]
    (not= (count all-corners)
          4)))

(defn orientation [edge]
  (let [[y x] (first edge)
        [a b] (second edge)]
    (if (= (- y a) 0)
      :horizontal
      :vertical)))


(defn join-edges [edges]
  (->> edges
       (reduce (fn [acc edge]
                  (let [e (->> acc (some (fn [e] (and (= (orientation e) (orientation edge))
                                                      (share-corner? e edge)
                                                      e))))
                        acc-without-e (->> acc (filter #(not= % e)))]
                    (if e
                      (conj acc-without-e (join-edge e edge))
                      (conj acc edge))))
               [])))

(defn calc-area [region] (count region))
(defn calc-perimeter [region]
  (let [edges-for-each-plant (->> region (mapcat edges))
        appears-only-once?   (fn [edge] (= 1 (count (filter #(= edge %) edges-for-each-plant))))
        perimeter            (->> edges-for-each-plant (filter appears-only-once?)
                                                       (join-edges))]
    (count perimeter)))

(->> regions
     (map #(* (calc-area %) (calc-perimeter %)))
     (reduce +))
