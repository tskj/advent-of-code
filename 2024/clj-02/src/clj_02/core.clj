(ns clj-02.core
  (:require
   [clojure.string :refer [split]]))

(def input-file "resources/example.txt")
(def input
  (->> input-file
       (slurp)
       (    #(split % #"\n"))
       (map #(split % #" "))
       (map #(map parse-long %))))

(defn diffs-of-report [report]
  (->> report
       (partition 2 1)
       (map (fn [[a b]] (- a b)))))

(defn is-safe [report]
  (let [diffs (diffs-of-report report)]
    (->> diffs
         (every? (fn [x] (<= (abs x) 3))))))

(defn is-monotonic [report]
  (let [diffs (diffs-of-report report)]
    (or (every? (fn [diff] (> diff 0)) diffs)
        (every? (fn [diff] (< diff 0)) diffs))))

(defn is-safe-and-mono [report]
  (and (is-monotonic report) (is-safe report)))

(defn generate-reports [report]
  (let [indices (range (count report))
        remove-idx (fn [idx]
                     (concat (subvec (vec report) 0 idx)
                             (subvec (vec report) (inc idx))))]
    (map remove-idx indices)))

(defn is-actually-safe [report]
  (let [candidate-reports (generate-reports report)]
    (or (is-safe-and-mono report)
        (some is-safe-and-mono candidate-reports))))

(->> input
     (map is-actually-safe)
     (filter true?)
     (count))

(defn tuple-part [s]
  (->> s
       (reduce (fn [[result curr] x]
                 (if (nil? curr)
                   [result x]
                   [(conj result [curr x]) x]))
               [[] nil])
       ((fn [[result _]] result))))

;; (tuple-part [1 2 3 4 5 6 7 8 9])

(->> [1 2 3 4 5 6 7 8 9]
     (reduce (fn [acc x] (let [l (last acc)]
                           (conj acc (conj l x))))
             [[]]))
