(ns rest-of-advent-24.day-03
  (:require
   [clojure.string :refer [starts-with?]]))

(defmacro blk
  {:clj-kondo/ignore true}
  ([] nil)
  ([& block]
   (assert (seq? block))
   (assert (> (count block) 0))
   (let [line-of-code (first block)
         rest-of-codeblock (rest block)]

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
                result#)))))))

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

(def input
  (->> "resources/day-03-input.txt"
       (slurp)))

(defn is-digit? [d]
  (Character/isDigit d))

(macroexpand
  '(blk
     (const-try [next-digits s'] (parse-n-digit-int (subs s 1)))
     (if (empty? s) {:return nil})
     (const first-character (first s))
     (if (not (is-digit first-character)) {:return nil})

     (println "parse-n called with " s " and first-character=" first-character)

     (const digit first-character)
     (const-try [next-digits s'] (parse-n-digit-int (subs s 1)))
     (println "next-digits " next-digits " and s'=" s')

     [(str digit (or next-digits "")) s']))

(defn parse-n-digit-int [s]
  (blk
   (if (empty? s) {:return nil})

   (const first-character (first s))
   (const r (subs s 1))

   (if (not (is-digit? first-character)) {:return nil})

   (const digit first-character)
   (const [next-digits s'] (or (parse-n-digit-int (subs s 1))
                               [nil r]))

   {:return [(str digit (or next-digits "")) s']}))

(defn str-to-int [s]
  (assert (every? is-digit? s) "this function only applies to int digits")
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

