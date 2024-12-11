(ns rest-of-advent-24.day-11
  (:require
   [clojure.string :refer [join split split-lines]]
   [rest-of-advent-24.utils.macros :refer [blk]]))

(def input
  (->> (slurp "resources/day-11-input.txt")
       (split-lines)
       (first)
       (#(split % #" "))
       (mapv parse-long)))

(defn split-number [n]
  (let [str-rep (str n)
        vec-rep (vec str-rep)
        len (count vec-rep)
        half-len (/ len 2)]
    (assert (even? len))
    (->> [(take half-len vec-rep) (drop half-len vec-rep)]
         (mapv #(join %))
         (mapv parse-long))))

(defn number-of-digits [n]
  (->> n
       (str)
       (count)))

(defn split-stones [n]
  (cond
    (= n 0) [1]
    (even? (number-of-digits n)) (split-number n)
      :else [(* 2024 n)]))

(defn create-lazy-tree [n]
  (fn []
    [n (mapv create-lazy-tree (split-stones n))]))

(defn run [f]
  (f))


(def memo (atom {}))

(defn calc-width-at-depth [tree depth]
  (let [[root rose] (run tree)]
    (case depth
      0 1
      1 (count rose)
      (blk
        (const key [root depth])
        (if-let [m (@memo key)] (return m))
        (const result
               (->> rose
                    (map #(calc-width-at-depth % (dec depth)))
                    (reduce +)))
        (swap! memo conj [key result])
        result))))

(time
  (->> input
       (map #(calc-width-at-depth (create-lazy-tree %) 75))
       (reduce +)))






; (defn blink-15 [n]
;   (or (@memo n)
;       (let [result (->> [n]
;                         (blink)
;                         (blink)
;                         (blink)
;                         (blink)
;                         (blink)
;                         (blink)
;                         (blink)
;                         (blink)
;                         (blink)
;                         (blink)
;                         (blink)
;                         (blink)
;                         (blink)
;                         (blink)
;                         (blink))]
;         (swap! memo conj [n result])
;         result)))

; (count (keys @memo))

; (->> input
;      (mapcat blink-15)
;      (mapcat blink-15)
;      (mapcat blink-15)
;      (mapcat blink-15)
;      (mapcat blink-15)
;      (take 10000000)
;      (vec)
;      (count))

; (->> (seq @memo)
;      (mapcat second)
;      (filter #(nil? (get @memo %))))

; (->> [0]
;      (mapcat @memo)
;      (mapcat @memo)
;      (mapcat @memo)
;      (mapcat @memo)
;      (count))
