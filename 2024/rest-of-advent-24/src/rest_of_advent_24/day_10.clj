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

(defn get-next-steps [coord]
  (let [potential-dirs (->> (map #(add coord %) directions)
                            (filter is-on-map?))
        current-height (get-in input coord)
        valid-dirs (->> potential-dirs 
                        (filter (fn [dir] (= 1 (- (get-in input dir) current-height)))))]
    valid-dirs))

(defn is-a-nine? [coord]
  (= 9 (get-in input coord)))

(defn number-of-9s [trail-head]
  (loop [nines #{}
         steps [trail-head]]
    (if (empty? steps)
      (count nines)
      (recur (into nines (filter is-a-nine? steps))
             (mapcat get-next-steps steps)))))


(->> trailhead-coords
     (map number-of-9s)
     (reduce +))
