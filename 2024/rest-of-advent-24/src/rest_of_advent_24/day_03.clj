(ns rest-of-advent-24.day-03
  (:require
   [clojure.string :refer [starts-with?]]))

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

(def input (slurp "resources/day-03-input.txt"))

(defn is-digit? [d]
  (Character/isDigit d))

(defn str-to-int [s]
  (assert (every? is-digit? s) "this function only applies to int digits")
  (->> [0 s]
       (apply reduce (fn [acc x]
                       (let [digit (- (int x) 48)]
                         (+ digit (* 10 acc)))))))

(defn parse-n-digit-int [s]
  (blk
   (if (empty? s) (return nil))

   (const first-character (first s))
   (const s (subs s 1))

   (if (not (is-digit? first-character)) (return nil))

   (const digit first-character)
   (const [next?-digits s] (or (parse-n-digit-int s)
                               ["" s]))

   [(str digit next?-digits) s]))

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

(defn parse-mul-thing [s]
  (blk
    (const-try [_mul        s] (parse-mul s))
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
      (blk
        (if-let [[result s] (parse-mul-thing s)]
          (recur (conj acc result) true s))

        (if-let [[don't s] (parse-don't s)]
          (recur acc false s))

        (recur acc true (subs s 1)))

      :else
      (blk
        (if-let [[do s] (parse-do s)]
          (recur acc true s))

        (recur acc false (subs s 1)))))

  (reduce +))
