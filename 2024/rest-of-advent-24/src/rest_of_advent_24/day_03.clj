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
               `(let [rhs# ~rhs
                      ~lhs rhs#]
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

(let [result__6860__auto__ "test"]
  (println result__6860__auto__)

  (blk (if (= (+ 1 1) 4) "yeah")
     (const a "hello")
     (println (str "hello___" result__6860__auto__))
     (if true (str "her " a))
     (if false "return this? no")
     a))

(macroexpand
  '(blk (const a "hello")
        (if false "return this? no")
        "hello"))

(blk (const a "hello")
     (if false "return this? no")
     "hello")

(blk (if (nil? "") "yeah")
     (if true {:return "test"})
     (const a nil)
     (if true "return this? no")
     a)

(blk (if (nil? "") "yeah")
     (const-try a "hei")
     (if false "return this? no")
     a)

(def input (slurp "resources/day-03-input.txt"))

(defn is-digit? [d]
  (Character/isDigit d))

(defn parse-n-digit-int [s]
  (blk
   (if (empty? s) {:return nil})

   (const first-character (first s))
   (const r (subs s 1))

   (if (not (is-digit? first-character)) {:return nil})

   (const digit first-character)
   (const [next-digits s'] (or (parse-n-digit-int (subs s 1))
                               [nil r]))

   [(str digit (or next-digits "")) s']))

(defn str-to-int [s]
  (assert (every? is-digit? s) "this function only applies to int digits")
  (->> [0 s]
       (apply reduce (fn [acc x]
                       (let [digit (- (int x) 48)]
                         (+ digit (* 10 acc)))))))

(defn parse-literal [p s]
  (assert (not (nil? s)) "string can't be null")
  (if (starts-with? s p)
    [p (subs s (count p))]))

(defn parse-comma [s]
  (assert (not (nil? s)) "comma s is nil")
  (parse-literal "," s))

(defn parse-ending-paren [s]
  (assert (not (nil? s)) "end-paren s is nil")
  (parse-literal ")" s))

(defn parse-do [s]
  (assert (not (nil? s)) "do s is nil")
  (parse-literal "do()" s))

(defn parse-don't [s]
  (assert (not (nil? s)) "don't s is nil")
  (parse-literal "don't()" s))

(def mul "mul(") ;; )

(defn parse-mul-thing [initial-s]
  (blk
    (const-try [_mul         s] (parse-literal mul initial-s))
    (const-try [the-int      s] (parse-n-digit-int s))
    (const-try [_comma       s] (parse-comma s))
    (const-try [the-next-int s] (parse-n-digit-int s))
    (const-try [_end-paren   s] (parse-ending-paren s))

    [(* (str-to-int the-int) (str-to-int the-next-int)) s]))

(->>

  (loop [acc []
         enabled true
         s input]

    (cond (empty? s)
          acc

          enabled
          (let [[result s] (or (parse-mul-thing s)
                               [nil s])
                [d s] (or (parse-don't s)
                          [nil s])]
            (if (nil? result)
              (recur acc (nil? d) (subs s 1))
              (recur (conj acc result) (nil? d) s)))

          :else
          (let [[d s] (or (parse-do s)
                          [nil s])
                next-s (if (nil? d) (subs s 1) s)]
            (recur acc (not (nil? d)) next-s))))

  (reduce +))

