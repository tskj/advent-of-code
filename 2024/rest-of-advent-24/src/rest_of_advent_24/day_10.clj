(ns rest-of-advent-24.day-10
  (:require
   [clojure.string :refer [split-lines]]))

(def input
  (->> (slurp "resources/day-10-input.txt")
       (split-lines)
       (mapv vec)
       (mapv #(mapv str %))
       (mapv #(mapv parse-long %))))

(def width (count (first input)))
(def height (count input))

(def trailhead-coords
  (->> input
    (keep-indexed (fn [y row] (->> row (keep-indexed (fn [x z] (when (= z 0) [y x]))))))
    (apply concat)))

(def directions [[-1 0] [0 1] [1 0] [0 -1]])
(defn add [[a b] [c d]]
  [(+ a c) (+ b d)])

(defn is-on-map? [[y x]]
  (and (< y height)
       (< x width)
       (>= y 0)
       (>= x 0)))

(defn get-next-steps [path]
  (let [coord (last path)
        potential-dirs (->> (map #(add coord %) directions)
                            (filter is-on-map?))
        current-height (get-in input coord)
        valid-dirs (->> potential-dirs
                        (filter (fn [dir] (= 1 (- (get-in input dir) current-height)))))]
    (->> valid-dirs (map (fn [dir] (conj path dir))))))

(defn is-a-nine? [path]
  (let [coord (last path)]
    (= 9 (get-in input coord))))

(defn number-of-9s [trail-head]
  (loop [nines #{}
         paths [[trail-head]]]
    (if (empty? paths)
      (count nines)
      (recur (into nines (filter is-a-nine? paths))
             (mapcat get-next-steps paths)))))


(->> trailhead-coords
     (map number-of-9s)
     (reduce +))
