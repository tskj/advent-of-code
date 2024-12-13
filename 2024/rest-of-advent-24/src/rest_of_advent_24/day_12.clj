(ns rest-of-advent-24.day-12
  (:require
   [clojure.string :refer [split-lines]]
   [clojure.test :refer [deftest is testing]]))

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

(def row-major-indices
  (->> indices
       (group-by first)
       (map second)))

(def col-major-indices
  (->> indices
       (group-by second)
       (map second)))

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
        (let [adjacent-indices (->> input (filter (fn [c] (->> region (some #(is-adjacent? c %)))))
                                          (set))]
          (if (empty? adjacent-indices)
            (recur (rest input)             ;; consume one
                   #{(first input)}         ;; start new region
                   (if (> (count region) 0) ;; put region aside
                     (conj regions region)
                     regions))
            (recur (->> input (remove adjacent-indices)) ;; consume all adjacent
                   (into region adjacent-indices)        ;; put in adjacent region
                   regions))))))

(def regions
  (time
    (->> regions-by-plant
         (map (fn [[k v]] v))
         (pmap create-regions)
         (apply concat))))

(def region-id-by-index
  (->> regions
       (map-indexed (fn [id region] (->> region (map (fn [idx] [idx id])))))
       (apply concat)
       (into {})))

(def fences-by-region-id
  (->> region-id-by-index
       (map second)
       (map (fn [id] [id 0]))
       (into {})
       (atom)))

(defn calc-area [region-id] (count (nth regions region-id)))
(defn calc-perimeter []
  (doseq [row row-major-indices]
    (let [previous-fence-above (atom nil)
          previous-fence-below (atom nil)]
      (doseq [coord row]
        (let [current-region-id (region-id-by-index coord)
              current-plant     (get-plant coord)
              plant-above       (get-plant (add coord [-1 0]))
              plant-below       (get-plant (add coord [1 0]))
              is-fence-above?   (not= current-plant plant-above)
              is-fence-below?   (not= current-plant plant-below)]

          (when (and is-fence-above? (not= @previous-fence-above current-plant))
            (swap! fences-by-region-id (fn [a] (update-in a [current-region-id] inc))))
          (when (and is-fence-below? (not= @previous-fence-below current-plant))
            (swap! fences-by-region-id (fn [a] (update-in a [current-region-id] inc))))

          (if is-fence-above?
            (reset! previous-fence-above current-plant)
            (reset! previous-fence-above nil))
          (if is-fence-below?
            (reset! previous-fence-below current-plant)
            (reset! previous-fence-below nil))))))

  (doseq [col col-major-indices]
    (let [previous-fence-left   (atom nil)
          previous-fence-right  (atom nil)]
      (doseq [coord col]
        (let [current-region-id (region-id-by-index coord)
              current-plant     (get-plant coord)
              plant-left        (get-plant (add coord [0 -1]))
              plant-right       (get-plant (add coord [0 1]))
              is-fence-left?    (not= current-plant plant-left)
              is-fence-right?   (not= current-plant plant-right)]

          (when (and is-fence-left? (not= @previous-fence-left current-plant))
            (swap! fences-by-region-id (fn [a] (update-in a [current-region-id] inc))))
          (when (and is-fence-right? (not= @previous-fence-right current-plant))
            (swap! fences-by-region-id (fn [a] (update-in a [current-region-id] inc))))

          (if is-fence-left?
            (reset! previous-fence-left current-plant)
            (reset! previous-fence-left nil))
          (if is-fence-right?
            (reset! previous-fence-right current-plant)
            (reset! previous-fence-right nil)))))))

(calc-perimeter)

(time
  (->> @fences-by-region-id
       (map (fn [[region-id fences-n]] (* fences-n (calc-area region-id))))
       (reduce +)))

; AAAAA|ZZZZZ
;      +-~~~~
; AAAAA|BBBBB      <- (begge tilfeller: prev-plant != curr-plant)
;       ^
;       prev-fence = false

; QQQQQ|ZZZZZ
; -~~~~+-~~~~
; AAAAA|BBBBB      <- (begge tilfeller: prev-plant != curr-plant)
;       ^
;       prev-fence = true
