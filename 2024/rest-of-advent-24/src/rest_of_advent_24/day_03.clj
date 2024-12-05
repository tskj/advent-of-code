(ns rest-of-advent-24.day-03
  (:require
   [clojure.string :refer [starts-with?]]))

(defmacro blk
  {:clj-kondo/ignore true}
  ([] nil)
  ([last-return] last-return)
  ([line-of-code & rest-of-codeblock]
   (cond (and (seq? line-of-code) (= 'const (first line-of-code))) ;; const
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

         :else ;; return first non-nil value
         `(let [result# ~line-of-code]
           (if (nil? result#)
             (blk ~@rest-of-codeblock)
             (if (and (map? result#) (= (set (keys result#)) #{:return}))
              (:return result#)
              result#))))))

(def input (slurp "resources/day-03-input.txt"))

(defn is-digit? [d]
  (Character/isDigit d))

(defn parse-n-digit-int [s]
  (blk
   (if (empty? s) {:return nil})

   (const first-character (first s))
   (const s (subs s 1))

   (if (not (is-digit? first-character)) {:return nil})

   (const digit first-character)
   (const [next-digits s] (or (parse-n-digit-int s)
                              [nil s]))

   [(str digit (or next-digits "")) s]))

(defn str-to-int [s]
  (assert (every? is-digit? s) "this function only applies to int digits")
  (->> [0 s]
       (apply reduce (fn [acc x]
                       (let [digit (- (int x) 48)]
                         (+ digit (* 10 acc)))))))

(defn parse-literal [p s]
  (when (starts-with? s p)
    [p (subs s (count p))]))

(defn parse-comma [s]
  (parse-literal "," s))

(defn parse-ending-paren [s]
  (parse-literal ")" s))

(defn parse-do [s]
  (parse-literal "do()" s))

(defn parse-don't [s]
  (parse-literal "don't()" s))

(defn parse-mul [s]
  (parse-literal "mul(" s))

(defn parse-mul-thing [initial-s]
  (blk
    (const-try [_mul        s] (parse-mul initial-s))
    (const-try [first-int   s] (parse-n-digit-int s))
    (const-try [_comma      s] (parse-comma s))
    (const-try [second-int  s] (parse-n-digit-int s))
    (const-try [_end-paren  s] (parse-ending-paren s))

    [(* (str-to-int first-int) (str-to-int second-int)) s]))

(->>

  (loop [acc []
         enabled? true
         s input]

    (cond
      (empty? s)
      acc

      enabled?
      (let [[result s] (or (parse-mul-thing s)
                           [nil s])
            [don't  s] (or (parse-don't s)
                           [nil (if (nil? result) (subs s 1) s)])]
        (if (nil? result)
          (recur acc                (nil? don't) s)
          (recur (conj acc result)  (nil? don't) s)))

      :else
      (let [[do s] (or (parse-do s)
                       [nil (subs s 1)])]
        (recur acc (not (nil? do)) s))))

  (reduce +))

