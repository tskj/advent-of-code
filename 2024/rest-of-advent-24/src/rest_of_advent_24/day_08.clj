(ns rest-of-advent-24.day-08
  (:require
   [clojure.string :refer [split-lines]]
   [rest-of-advent-24.utils.macros :refer [blk]]))

(def input
  (->> (slurp "resources/day-08-input.txt")
       (split-lines)))

(def unique-letters
  (blk
    (const u (atom #{}))
    (doseq [line input]
      (doseq [letter line]
        (when (not= letter \.)
          (swap! u conj letter))))

    @u))

(defn is-on-map? [[y x]]
  (blk
    (if (< y 0) (return false))
    (if (< x 0) (return false))
    (if (>= y (count input)) (return false))
    (assert (> (count input) 0))
    (assert (every? (fn [ll] (= ll (count (first input)))) (map count input)))
    (if (>= x (count (first input))) (return false))
    true))

(defn antenna-coords-kv [ul]
  (blk
    (const coords (atom #{}))
    (const y (atom 0))
    (doseq [line input]
      (blk
        (const x (atom 0))
        (doseq [letter line]
          (blk
            (when (= letter ul)
              (swap! coords conj [@y @x]))
            (swap! x inc)))
        (swap! y inc)))

    [ul @coords]))

(def antenna-with-their-coords
  (->> unique-letters
       (map antenna-coords-kv)
       (into {})))

(defn minus [[y1 x1] [y2 x2]]
  [(- y1 y2) (- x1 x2)])

(defn add [[y1 x1] [y2 x2]]
  [(+ y1 y2) (+ x1 x2)])

(defn mul [scalar [y x]]
  [(* 2 y) (* 2 x)])

(defn antinode-coords [antenna-coords]
  (blk
    (const antinodes (atom []))
    (doseq [yx antenna-coords]
      (doseq [other antenna-coords]
        (when (not= yx other)
          (blk
            (println "doing stuff" yx)
            (const dir (minus other yx))
            (const antinode (atom (add yx dir)))
            (loop [x nil]
              (println @antinodes)
              (when (is-on-map? @antinode)
                (swap! antinodes conj @antinode)
                (recur nil)))
            (const antinode* (atom (add yx (mul -1 dir))))
            (loop [x nil]
              (when (is-on-map? @antinode*)
                (swap! antinodes conj @antinode*)
                (recur nil)))))))
    @antinodes))

(->> antenna-with-their-coords
     (mapcat (fn [[a c]] (antinode-coords c)))
     (set)
     (count))
