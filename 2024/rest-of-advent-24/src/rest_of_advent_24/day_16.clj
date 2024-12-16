(ns rest-of-advent-24.day-16
  (:require
   [clojure.string :refer [split split-lines]]
   [rest-of-advent-24.utils.macros :refer [blk]]))

(def input
  (->> (slurp "resources/day-16-input.txt")
       (split-lines)))

(def width (count (first input)))
(def height (count input))

(defn is-wall? [p]
  (= \# (get-in input p)))

(defn is-start? [p]
  (= \S (get-in input p)))

(defn is-end? [p]
  (= \E (get-in input p)))

(defn is-empty? [p]
  (or (= \. (get-in input p))
      (is-start? p)
      (is-end? p)))

(def start-coord
  (let [c (atom nil)]
    (doseq [y (range height)]
      (doseq [x (range width)]
        (when (is-start?  [y x])
          (reset! c [y x]))))
    (assert (some? @c))
    @c))

(def end-coord
  (let [c (atom nil)]
    (doseq [y (range height)]
      (doseq [x (range width)]
        (when (is-end? [y x])
          (reset! c [y x]))))
    (assert (some? @c))
    @c))


(defn is-on-map? [m p]
  (let [[y x] p]
    (and (< y height)
         (< x width)
         (>= y 0)
         (>= x 0))))

(defn rev-dir [dir]
  (case dir
    :up     :down
    :right  :left
    :down   :up
    :left   :right))

(defn turn-right [dir]
  (case dir
    :up     :right
    :right  :down
    :down   :left
    :left   :up))

(defn turn-left [dir]
  (case dir
    :up     :left
    :right  :up
    :down   :right
    :left   :down))

(defn add [[y x] dir]
  (case dir
    :up     [(dec y) x]
    :right  [y (inc x)]
    :down   [(inc y) x]
    :left   [y (dec x)]))

(defn increase-score-by-turning [score]
  (+ score 1000))

(defn turn-reindeer-right [reindeer]
  (-> reindeer
      (update :dir turn-right)
      (update :score increase-score-by-turning)
      (update :path conj :clockwise)))

(defn turn-reindeer-left [reindeer]
  (-> reindeer
      (update :dir turn-left)
      (update :score increase-score-by-turning)
      (update :path conj :anti-clockwise)))

(defn turn-reindeer-around [reindeer]
  (-> reindeer
      (update :dir rev-dir)
      (update :score increase-score-by-turning)
      (update :score increase-score-by-turning)
      (update :path conj :turn-around)))

(defn advance-reindeer [reindeer]
  (assert (is-empty? (:pos reindeer)))
  (assert (is-empty? (add (:pos reindeer) (:dir reindeer))))
  (-> reindeer
      (update :pos #(add % (:dir reindeer)))
      (update :score inc)
      (update :path conj (:dir reindeer))))

(defn reindeer-stanger-i-veggen? [reindeer]
  (is-wall? (add (:pos reindeer) (:dir reindeer))))

(def branching-coords?
  (let [nodes (atom #{})]
    (doseq [y (range height)]
      (doseq [x (range width)]
        (let [c     [y x]
              north (add c :up)
              east  (add c :right)
              south (add c :down)
              west  (add c :left)]
          (when (is-empty? c)
            (when (<= 3 (reduce + [(if (is-empty? north) 1 0)
                                   (if (is-empty? east)  1 0)
                                   (if (is-empty? south) 1 0)
                                   (if (is-empty? west)  1 0)]))
              (swap! nodes conj c))))))
    @nodes))

(defn branch-reindeers [reindeer]
  (if (is-end? (:pos reindeer))
    [reindeer]
    (let [left-looking  (turn-reindeer-left reindeer)
          right-looking (turn-reindeer-right reindeer)]
      (->> [reindeer left-looking right-looking]
           (remove reindeer-stanger-i-veggen?)))))

(defn walk-until-decision-point [reindeer]
  (loop [reindeer (if (branching-coords? (:pos reindeer))
                    (advance-reindeer reindeer)
                    reindeer)]
    (cond
      (is-end? (:pos reindeer))             reindeer
      (branching-coords? (:pos reindeer))    reindeer
      (reindeer-stanger-i-veggen? reindeer) (let [left-looking-reindeer (turn-reindeer-left reindeer)
                                                  right-looking-reindeer (turn-reindeer-right reindeer)
                                                  backwards-reindeer (turn-reindeer-around reindeer)]
                                              (cond
                                                (not (reindeer-stanger-i-veggen? left-looking-reindeer))
                                                (do
                                                  (assert (reindeer-stanger-i-veggen? right-looking-reindeer))
                                                  (recur left-looking-reindeer))

                                                (not (reindeer-stanger-i-veggen? right-looking-reindeer))
                                                (do
                                                  (assert (reindeer-stanger-i-veggen? left-looking-reindeer))
                                                  (recur right-looking-reindeer))

                                                :else
                                                (do
                                                  (assert (reindeer-stanger-i-veggen? right-looking-reindeer))
                                                  (assert (reindeer-stanger-i-veggen? left-looking-reindeer))
                                                  (recur backwards-reindeer))))


      :else
      (recur (advance-reindeer reindeer)))))

(def node-cost-map (atom {}))

(defn reindeer-key [r]
  (select-keys r [:pos :dir]))

(defn tee [f x]
  (f x)
  x)

(loop [reindeers (mapcat branch-reindeers [{:pos start-coord :dir :right :score 0 :path []}])]
  (let [next-reindeers (->> (map walk-until-decision-point reindeers)
                            (mapcat branch-reindeers)
                            (remove (fn [r] (let [k (reindeer-key r)
                                                  x (@node-cost-map k)]
                                              (if (and (some? x)
                                                       (> (:score r) x))
                                                true
                                                (do
                                                  (swap! node-cost-map assoc k (:score r))
                                                  false)))))
                            (vec))]
    (if (every? is-end? (map :pos next-reindeers))
      (do
        (println next-reindeers)
        (->> (map :score next-reindeers)
             (apply min)))
      (recur next-reindeers))))
