(ns rest-of-advent-24.day-07
  (:require
   [clojure.math :refer [pow]]
   [clojure.string :refer [split split-lines trim]]
   [rest-of-advent-24.utils.macros :refer [blk]]))

(def input
  (->> (slurp "resources/day-07-input.txt")
       (split-lines)
       (mapv (fn [line]
               (blk
                 (const [lhs rhs] (split line #":"))
                 (const numbers (->> (split rhs #" ") (mapv trim) (filter (comp not empty?))))
                 [(parse-long lhs) (mapv parse-long numbers)])))))

(defn int-to-binary-seq [n]
  (seq (Integer/toString n 3)))

(defn number-of-digits-base-10 [n]
  (loop [i 1
         antall-siffer 0]
    (if (> i n)
      antall-siffer
      (recur (* i 10) (inc antall-siffer)))))

(defn my-power [base exp]
  (loop [i 0
         result 1]
    (if (< i exp)
      (recur (inc i) (* result base))
      result)))

(defn concatenation [x y]
  (+ (* x (my-power 10 (number-of-digits-base-10 y)))
     y))

(defn get-plus-mul-combination [n m]
  (let [binary-string (int-to-binary-seq n)
        diff (- m (count binary-string))
        leading-zeroes (->> (range diff) (map (fn [_] \0)))]
      (assert (>= diff 0))
      (->> (concat leading-zeroes binary-string)
           (map (fn [x] (case x
                          \0 +
                          \1 *
                          \2 concatenation))))))

(defn is-equal? [equation]
  (fn [operators]
    (blk
      (const [result operands] equation)
      (assert (= (count operands) (inc (count operators))))
      (assert (>= (count operands) 2))
      (const zipped (map vector operators (rest operands)))
      (const sum (reduce (fn [acc [op x]] (op acc x))
                         (first operands) zipped))
      (= result sum))))

(defn is-valid? [equation]
  (blk
    (const [result rhs] equation)
    (const length (- (count rhs) 1))
    (const real-length (my-power 3 length))
    (if (->> (range real-length)
           (map (fn [n] (get-plus-mul-combination n length)))
           (some (is-equal? equation)))
        result
        0)))

(->> (mapv is-valid? input)
     (reduce +))
