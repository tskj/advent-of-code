(ns rest-of-advent-24.day-04
  (:require
   [clojure.pprint :as pprint]
   [clojure.string :refer [join split-lines starts-with?]]
   [rest-of-advent-24.utils.macros :refer [blk]]))

(defn count-occurences [s sub]
  (blk
    (if (empty? s) (return 0))
    (const occured     (if (starts-with? s sub) 1 0))
    (const occurences  (count-occurences (subs s 1) sub))
    (+ occured occurences)))

(assert (= 3 (count-occurences "XMASXMASXMAS" "XMAS")))
(assert (= 0 (count-occurences "SAMXSAMXSAMX" "XMAS")))

(defn sub-matrix [m]
  (let [rest-rows (rest m)]
    (map rest rest-rows)))

(defn get-diag [m]
  (blk
    (if (= 0 (count m))
      (return '()))

    (const row (first m))

    (if (= 0 (count row))
      (return '()))

    (cons (first row)
          (get-diag (sub-matrix m)))))

(defn get-main-diagonals [m]
    (let [lower-diagonals (loop [acc [(rest m)]]
                           (let [curr (last acc)]
                             (if (empty? curr) acc
                               (recur (conj acc (rest curr))))))
          upper-diagonals (loop [acc [m]]
                           (let [curr (last acc)]
                             (if (empty? (first curr)) acc
                               (recur (conj acc (map rest curr))))))]
      (map get-diag (concat lower-diagonals upper-diagonals))))

(defn get-all-horizontal-subs [m]
  (loop [acc [m]]
    (let [curr (last acc)]
      (if (empty? (first curr)) acc
        (recur (conj acc (map rest curr)))))))

(defn get-subs [m]
  (if (empty? m) m
    (concat (get-all-horizontal-subs m)
            (get-subs (rest m)))))

(defn has-x-top-left [m]
  (blk
    (if (< (count m) 3)
      (return false))

    (const [row1 row2 row3 & rest] m)
    (assert (= (count row1)
               (count row2)
               (count row3)))

    (if (< (count row1) 3)
      (return false))

    (const [a _ b] row1)
    (const [_ c _] row2)
    (const [d _ e] row3)

    (or (and (= (str a c e) "SAM")
             (= (str b c d) "SAM"))
        (and (= (str b c d) "SAM")
             (= (str e c a) "SAM"))
        (and (= (str e c a) "SAM")
             (= (str d c b) "SAM"))
        (and (= (str d c b) "SAM")
             (= (str a c e) "SAM")))))
(def input
  (->> (slurp "resources/day-04-input.txt")
       (split-lines)
       (mapv seq)))

(->>
  (get-subs input)
  (map has-x-top-left)
  (filter (fn [x] x))
  (count))

(def rows input)
(def cols (apply map vector input))
(def main-diagonals (get-main-diagonals input))
(def other-diagonals (get-main-diagonals (reverse input)))

(def all-lines
  (map join
    (mapcat (fn [x] [x (reverse x)])
        (concat rows
                cols
                main-diagonals
                other-diagonals))))

(->>
  all-lines
  (map #(count-occurences % "XMAS"))
  (reduce +))

