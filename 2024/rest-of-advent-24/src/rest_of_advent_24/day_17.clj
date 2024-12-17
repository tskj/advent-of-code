(ns rest-of-advent-24.day-17
  (:require
   [clojure.string :refer [join] :as s]
   [clojure.test :refer [deftest is]]
   [rest-of-advent-24.utils.macros :refer [blk]]))

(def input-program
  (->> (slurp "resources/test/day-17-input.txt")))

(defn parse-literal [literal s]
  (blk
    (const-try does-it? (or (s/starts-with? s literal) nil))
    (const rest-of-s (subs s (count literal)))
    (assert (= s (str literal rest-of-s)))
    [literal rest-of-s]))

(deftest my-test-thingy
  (is (= ["test" "hei"] (parse-literal "test" "testhei")))
  (is (= nil (parse-literal "test" "tesXhei"))))

(defn is-digit? [d]
  (Character/isDigit d))

(defn parse-n-digits [s]
  (blk
   (if (empty? s) (return nil))

   (const first-character (first s))
   (const s (subs s 1))

   (if (not (is-digit? first-character)) (return nil))

   (const digit first-character)
   (const [next?-digits s] (or (parse-n-digits s)
                               ["" s]))

   (const digits (str digit next?-digits))

   [digits s]))

(defn parse-n-digit-int [s]
  (blk
    (const-try [digits s] (parse-n-digits s))
    (const-try int' (parse-long digits))
    [int' s]))

(deftest test-parse-digits
  (is (= 23 (first (parse-n-digit-int "23abcd"))))
  (is (= 234 (first (parse-n-digit-int "234abcd"))))
  (is (= 101 (first (parse-n-digit-int "101abcd"))))
  (is (= nil (parse-n-digit-int "a234abcd"))))

(defn parse-sequence-of-ints [s]
  (blk
    (const comma (parse-literal "," s))
    (if (nil? comma) (return []))

    (const [_ s] comma)

    (const-try [the-int s] (parse-n-digit-int s))
    (const-try [rest s]    (parse-sequence-of-ints s))
    [(conj rest the-int) s]))

(defn parse-program [s]
  (blk
    (const-try [_ s] (parse-literal "Register A: " s))
    (const-try [A s] (parse-n-digit-int s))
    (const-try [_ s] (parse-literal "\nRegister B: " s))
    (const-try [B s] (parse-n-digit-int s))
    (const-try [_ s] (parse-literal "\nRegister C: " s))
    (const-try [C s] (parse-n-digit-int s))

    (const-try [_ s] (parse-literal "\n\n" s))
    (const-try [_ s] (parse-literal "Program: " s))

    (const-try [first-int s] (parse-n-digit-int s))
    (const-try [rest-of-ints s] (parse-sequence-of-ints s))

    (const memory (conj rest-of-ints first-int))
    {:cpu {:a A :b B :c C :instruction-pointer 0} :memory (vec memory) :output []}))

(defn power-of-2 [x]
  (loop [x x
         p 1]
    (if (<= x 0)
       p
       (recur (dec x) (* p 2)))))

(defn eval-combo [computer operand]
  (case operand
    0 0
    1 1
    2 2
    3 3
    4 (->> computer :cpu :a)
    5 (->> computer :cpu :b)
    6 (->> computer :cpu :c)
    7 (assert false "unreachable according to spec")))

(defn step [computer]
  (println computer)
  (let [p (->> computer :cpu :instruction-pointer)
        opcode  (->> computer :memory (#(nth % p)))
        operand (->> computer :memory (#(nth % (inc p))))
        advptr  (fn [c*] (update-in c* [:cpu :instruction-pointer] (c* inc inc)))]
    (case opcode
      0         (let [numerator (->> computer :cpu :a)
                      denominator (->> (power-of-2 (eval-combo computer operand)))]
                  (->> (int (/ numerator denominator))
                       (assoc-in computer [:cpu :a])
                       (advptr)))

      1         (let [b (->> computer :cpu :b)]
                  (->> (bit-xor b operand)
                       (assoc-in computer [:cpu :b])
                       (advptr)))

      2         (let [combo (eval-combo computer operand)]
                  (->> (mod combo 8)
                       (assoc-in computer [:cpu :b])
                       (advptr)))

      3         (let [a (->> computer :cpu :a)]
                  (if (= a 0)
                    (->> computer
                         (advptr))
                    (-> computer
                        (assoc-in [:cpu :instruction-pointer] operand))))

      4         (let [b (->> computer :cpu :b)
                      c (->> computer :cpu :c)]
                  (->> (bit-xor b c)
                       (assoc-in computer [:cpu :b])
                       (advptr)))

      5         (let [combo (eval-combo computer operand)]
                  (->> (mod combo 8)
                       (update-in computer [:output] conj)
                       (advptr)))

      6         (let [numerator (->> computer :cpu :a)
                      denominator (->> (power-of-2 (eval-combo computer operand)))]
                  (->> (int (/ numerator denominator))
                       (assoc-in computer [:cpu :b])
                       (advptr)))

      7         (let [numerator (->> computer :cpu :a)
                      denominator (->> (power-of-2 (eval-combo computer operand)))]
                  (->> (int (/ numerator denominator))
                       (assoc-in computer [:cpu :c])
                       (advptr))))))

(defn has-halted? [computer]
  (let [p (->> computer :cpu :instruction-pointer)
        m (->> computer :memory)]
    (>= p (dec (count m)))))

(defn run-computer [computer]
  (loop [c computer]
    (let [nc (step c)]
      (if (has-halted? nc)
        nc
        (recur nc)))))

(def computer (run-computer (parse-program input-program)))
computer

(->>
  computer
  :output
  (join ","))

(deftest test-cases-from-part-1
  (is (= 1 (:b (:cpu (run-computer {:cpu {:a 0 :b 0 :c 9 :instruction-pointer 0}
                                    :memory [2 6]
                                    :output []})))))
  (is (= [0 1 2] (:output (run-computer {:cpu {:a 10 :b 0 :c 0 :instruction-pointer 0}
                                         :memory [5 0 5 1 5 4]
                                         :output []}))))
  (is (= [4 2 5 6 7 7 7 7 3 1 0] (:output (run-computer {:cpu {:a 2024 :b 0 :c 0 :instruction-pointer 0}
                                                         :memory [0 1 5 4 3 0]
                                                         :output []}))))
  (is (= 26 (:b (:cpu (run-computer {:cpu {:a 0 :b 29 :c 0 :instruction-pointer 0}
                                     :memory [1 7]
                                     :output []})))))
  (is (= 44354 (:b (:cpu (run-computer {:cpu {:a 0 :b 2024 :c 43690 :instruction-pointer 0}
                                        :memory [4 0]
                                        :output []})))))
  (is (= [4 6 3 5 6 3 5 2 1 0] (:output (run-computer (parse-program (slurp "resources/test/day-17-input.txt")))))))
