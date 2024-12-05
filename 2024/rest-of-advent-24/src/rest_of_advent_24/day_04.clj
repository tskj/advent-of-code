(ns rest-of-advent-24.day-04
  (:require
   [clojure.pprint :as pprint]
   [clojure.string :refer [join split-lines starts-with?]]))

(defn is-explicit-return? [r]
  (and (seq? r)
       (= (first r) 'return)))

(defn unwrap-explicit-return [r]
  (if (is-explicit-return? r)
      (do (assert (= (count r) 2) "return needs exactly one return value")
          (second r))
      r))

(defmacro blk
  {:clj-kondo/ignore true}
  ([] nil)
  ([last-return] (unwrap-explicit-return last-return))
  ([line-of-code & rest-of-codeblock]
   (cond (and (seq? line-of-code) (= 'const (first line-of-code)))
         (do (assert (= 3 (count line-of-code)))
             (let [[_ lhs rhs] line-of-code]
               `(let [~lhs ~rhs]
                  (blk ~@rest-of-codeblock))))

         (and (seq? line-of-code) (= 'const-try (first line-of-code)))
         (do (assert (= 3 (count line-of-code)))
             (let [[_ lhs rhs] line-of-code]
               `(let [rhs# ~rhs]
                  (if (nil? rhs#)
                    nil
                    (let [~lhs rhs#]
                      (blk ~@rest-of-codeblock))))))

         (and (seq? line-of-code) (= 'if (first line-of-code)))
         (do (assert (= 3 (count line-of-code)))
             (let [[if' cond r] line-of-code]
               (list if' cond (unwrap-explicit-return r) `(blk ~@rest-of-codeblock))))

         (and (seq? line-of-code) (= 'if-let (first line-of-code)))
         (do (assert (= 3 (count line-of-code)))
             (let [[if-let' cond r] line-of-code]
               (list if-let' cond (unwrap-explicit-return r)`(blk ~@rest-of-codeblock))))

         :else
         (if (is-explicit-return? line-of-code)
           (unwrap-explicit-return line-of-code) ;; <- returns explicit return, useful for debugging
           `(do line-of-code                     ;; <- sideeffect
                (blk ~@rest-of-codeblock))))))


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

(def input
  (->> (slurp "resources/day-04-input.txt")
       (split-lines)
       (mapv seq)))

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

