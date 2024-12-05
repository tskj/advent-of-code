(ns rest-of-advent-24.day-05
  (:require
   [clojure.string :refer [split split-lines]]))

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
           `(do ~line-of-code                     ;; <- sideeffect
                (blk ~@rest-of-codeblock))))))

(def page
  (->> (slurp "resources/day-05-input.txt")
       (split-lines)))

(def rules
  (->>
    page
    (partition-by empty?)
    (first)
    (map #(split % #"\|"))
    (map (fn [x] (map parse-long x)))))

(def updates
  (->>
    page
    (partition-by empty?)
    (last)
    (map #(split % #","))
    (map (fn [x] (map parse-long x)))))

(defn includes [x coll]
  (some #(= % x) coll))

(defn rule-is-not-violated? [rule update]
  (let [numbers-in-update (->> update (filter (fn [x] (includes x rule))))]
    (assert (= (count numbers-in-update)
               (count rule)))
    (= rule numbers-in-update)))

(defn is-acceptable? [update]
  (let [applicable-rules (->> rules (filter (fn [[a b]] (and (includes a update)
                                                             (includes b update)))))]
    (every? #(rule-is-not-violated? % update) applicable-rules)))

(defn middle-number [update]
  (if (<= (count update) 1)
    (do (assert (= (count update) 1))
        (first update))
    (middle-number (->> update
                        (rest)
                        (reverse)
                        (rest)
                        (reverse)))))

(->> updates
     (filter is-acceptable?)
     (map middle-number)
     (reduce +))
