(ns rest-of-advent-24.day-03
  (:require
   [clojure.string :refer [split starts-with?]]))

(def input
  (->> "resources/day-03-input.txt"
       (slurp)))

(defn parse-n-digit-int [s]
  (if (Character/isDigit (first s))
    (let [digit (first s)
          [next-digits s'] (parse-n-digit-int (subs s 1))]
      [(str digit (or next-digits "")) s'])
    [nil s]))

(defn str-to-int [s]
  (assert (every? #(Character/isDigit %) s) "this function only applies to int digits")
  (reduce (fn [acc x]
            (let [digit (- (int x) 48)]
              (+ digit (* 10 acc))))
    0 s))

(defn parse-comma [s]
  (if (= \, (first s))
    ["," (subs s 1)]
    [nil s]))

(defn parse-ending-paren [s]
  (if (= \) (first s))
    [")" (subs s 1)]
    [nil s]))

(defn parse-do [s]
  (if (starts-with? s "do()")
    ["do()" (subs s 4)]
    [nil s]))

(defn parse-don't [s]
  (if (starts-with? s "don't()")
    ["don't()" (subs s 7)]
    [nil s]))

(defn parse-mul-thing [initial-s]
  (cond (< (count initial-s) 4)
        [nil initial-s]

        (not (starts-with? initial-s "mul("))
        [nil initial-s]

        :else
        (let [s (subs initial-s 4)
              [the-int s] (parse-n-digit-int s)]

          (cond (nil? the-int)
                [nil initial-s]

            :else
            (let [[_comma s] (parse-comma s)]
              (cond (nil? _comma)
                    [nil initial-s]

               :else
               (let [[the-next-int s] (parse-n-digit-int s)]
                 (cond (nil? the-next-int)
                       [nil initial-s]

                   :else
                   (let [[_paren s] (parse-ending-paren s)]
                     (if (nil? _paren)
                       [nil initial-s]
                       [(* (str-to-int the-int) (str-to-int the-next-int)) s]))))))))))

(->>

  (loop [acc []
         enabled true
         s input]
    (if (empty? s)
      acc
      (if enabled
        (let [[result s] (parse-mul-thing s)
              [d s] (parse-don't s)]
          (if (nil? result)
            (recur acc (nil? d) (subs s 1))
            (recur (conj acc result) (nil? d) s)))
        (let [[d s] (parse-do s)
              next-s (if (nil? d) (subs s 1) s)]
          (recur acc (not (nil? d)) next-s)))))

  (reduce +))

