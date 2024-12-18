(ns rest-of-advent-24.day-17
  (:require
   [clojure.string :refer [join] :as s]
   [clojure.test :refer [deftest is]]
   [rest-of-advent-24.utils.macros :refer [blk]]))

(def input-program
  (->> (slurp "resources/day-17-input.txt")))

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
       (recur (dec' x) (*' p 2)))))

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
  (let [p (->> computer :cpu :instruction-pointer)
        opcode  (->> computer :memory (#(nth % p)))
        operand (->> computer :memory (#(nth % (inc p))))
        advptr  (fn [c*] (update-in c* [:cpu :instruction-pointer] (comp inc inc)))]
    (case opcode
      0         (let [numerator (->> computer :cpu :a)
                      denominator (->> (power-of-2 (eval-combo computer operand)))]
                  (->> (bigint (/ numerator denominator))
                       (assoc-in computer [:cpu :a])
                       (advptr)))

      1         (let [b (->> computer :cpu :b)]
                  (->> (bit-xor (long b) operand)
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
                  (->> (bit-xor (long b) (long c))
                       (assoc-in computer [:cpu :b])
                       (advptr)))

      5         (let [combo (eval-combo computer operand)]
                  (->> (mod combo 8)
                       (update-in computer [:output] conj)
                       (advptr)))

      6         (let [numerator (->> computer :cpu :a)
                      denominator (->> (power-of-2 (eval-combo computer operand)))]
                  (->> (bigint (/ numerator denominator))
                       (assoc-in computer [:cpu :b])
                       (advptr)))

      7         (let [numerator (->> computer :cpu :a)
                      denominator (->> (power-of-2 (eval-combo computer operand)))]
                  (->> (bigint (/ numerator denominator))
                       (assoc-in computer [:cpu :c])
                       (advptr))))))

(defn has-halted? [computer]
  (let [p (->> computer :cpu :instruction-pointer)
        m (->> computer :memory)]
    (>= p (dec (count m)))))

(def lru-capacity 100000)
(def recently-used (atom []))
(def previous-state (atom {}))

(defn read-lru [k]
  (swap! recently-used conj k)
  (when (> (count @recently-used) lru-capacity)
    (swap! recently-used rest))
  (@previous-state k))

(defn write-lru [k v]
  (when (> (count @previous-state) lru-capacity)
    (swap! previous-state select-keys @recently-used))
  (swap! previous-state assoc k v)
  nil)

(def number-of-cache-hits (atom 0))
(def number-of-cache-misses (atom 0))

(defn starts-with?v [main-v sub-v]
  (= sub-v (subvec main-v 0 (count sub-v))))

(def corrupted-computer (parse-program input-program))

(defn run-computer [computer]
  (let [past-states (atom {(:cpu computer) computer})]
    (loop [c computer]
      (if-not (starts-with?v (:output c) (:output corrupted-computer))
        (let [borked-computer (assoc c :output [])]
          borked-computer)
        (if-let [c* (read-lru (:cpu c))]
          (let [c** (-> c* (assoc :output (vec (concat (:output c) (:output c*)))))]
            (doseq [[k v] @past-states]
               (let [old-output (vec (:output v))
                     new-output (vec (:output c**))
                     actual-output (vec (drop (count old-output) new-output))]
                 (assert (= old-output (subvec new-output 0 (count old-output))))
                 (write-lru k (assoc v :output actual-output))))

            (write-lru (:cpu computer) c**)
            (swap! number-of-cache-hits inc)
            c**)

          (let [nc (step c)]
            (if (@past-states (:cpu nc))
              (do
                (assert false "loop detetcted sadge")
                computer)
              (do
                (swap! past-states assoc (:cpu nc) nc)
                (if (has-halted? nc)
                  (do
                    ; (->> (vec @past-states)
                    ;      (mapv (fn [k] [k nc]))
                    ;      (into {})
                         (doseq [[k v] @past-states]
                            (let [old-output (vec (:output v))
                                  new-output (vec (:output nc))
                                  actual-output (vec (drop (count old-output) new-output))]
                              (assert (= old-output (subvec new-output 0 (count old-output))))
                              (write-lru k (assoc v :output actual-output))))
                         (write-lru (:cpu computer) nc)
                         (swap! number-of-cache-misses inc)
                    nc)
                  (recur nc))))))))))


(defn run-computer-until-first-output [computer]
    (let [start-output (:output computer)]
      (loop [c computer]
        (if (has-halted? c)
          c
          (let [next (step c)
                output (:output next)]
            (if (> (count output) (count start-output))
                next
                (recur next)))))))

; a'%8 -> b'''
; b'''^3 -> b''
; a'/2**b'' -> c''
; a'/2**3 -> a
; b''^c'' -> b'
; b'^5 -> b
; b%8 -> output
; a!=0? loop


(defn backwards* [a o]
  (->> (range 0 8)
       (mapcat
         (fn [n]
           (let [a' (+ (* a 8) n)]
              (assert (= a (bigint (/ a' 8))))
              (let [b''' (mod a' 8)
                    b'' (bit-xor (long b''') 3)
                    c'' (bigint (/ a' (power-of-2 b'')))
                    b' (bit-xor (long b'') (long c''))
                    b (bit-xor (long b') 5)]
                (if (= (mod b 8) o)
                    [a']
                    [])))))))

(time
  (loop [as [(bigint 0)]
         memory (reverse (:memory corrupted-computer))]
    (if (empty? memory) 
      as
      (let [current-m (first memory)] 
        (recur (mapcat (fn [a] (backwards* a current-m)) as)
               (rest memory)))))) 

; 236581108670061
; 236581108670143

; a o er "input"
; b = o, o+8, o+16, o+24, ...
; b' = b^5

; a* = a - (mod a 8)
; oops wrong, flip addition and multiplication step:
; a' = a* * 8, (a*+1) * 8, (a*+2) * 8, ... (a*+7) * 8
; eksempel: a = 6
;           48 / 8 -> 6
;           49 / 8 -> 6
;           50 / 8 -> 6
;           ...
;           55 / 8 -> 6
; (assert (int a'/8) == a)

; b''' = (mod a' 8)
; b'' = b'''^3
; c'' = (int a'/2**b'')
; b' = b''^c''
; b = b'^5
; does (mod b 8) == o?


; c'' = b'^b''

; assert c'' == a' * 2**b''
; assert (int a'/2**b'') == c''


(defn teest [a]
  (blk
    (const b (mod a 8))
    (const b (bit-xor (long b) 3))
    (const c (bigint (/ a (power-of-2 b))))
    (const a (bigint (/ a (power-of-2 3))))
    (const b (bit-xor (long b) (long c)))
    (const b (bit-xor (long b) 5))
    (const o (mod b 8))
    [a o]))

(teest 1)
(teest 2)
(teest 3)
(teest 4)
(teest 5)
(= (teest 6) [0 0])
(teest 7)


(defn arg-eql [f args output]
  (if (empty? args)
    (assert false (str "no argument produces output: " output))
    (let [arg (first args)]
      (if (= (f arg) output)
        arg
        (recur f (rest args) output)))))

(->> (range 0 199)
     (#(arg-eql teest % [6 3])))

(arg-eql teest (map (partial + 0) (range 0 8)) [0 0])

(loop [backwards-memory (reverse (:memory corrupted-computer))
       a 0]
  (println a backwards-memory)
  (let [current-mem (first backwards-memory)]
    (if-not current-mem
      a
      (recur (rest backwards-memory)
             (arg-eql teest (map (partial + a) (range)) [a current-mem])))))


; a = 6
; b = 0b110 (6)
; 3 = 0b011
; 6 / 64 -> c = 0
; a == 6
; a = 0
; c == 0
; b = 5 (6^0)
; 5 = 0b101
; b == 0

; 2,4,1,3,7,5,0,3,4,1,1,5,5,5,3,0


; (run-computer-until-first-output (assoc-in corrupted-computer [:cpu :a] 117440))

; (->> (range 0 100000)
;      (map (fn [a] [a (run-computer-until-first-output
;                        (run-computer-until-first-output
;                          (run-computer-until-first-output
;                            (run-computer-until-first-output
;                              (run-computer-until-first-output (assoc-in corrupted-computer [:cpu :a] a))))))]))
;      (filter (fn [[a c]] (<= 3 (count (:output c)))))
;      (filter (fn [[a c]] (= (subvec (:output c) 0 3)
;                             (subvec (:memory c) 0 3))))
;      (map (fn [[a c]] [a (mod a 8) c])))

; (time
;   (->>
;     (range 100000000 200000000)
;     (map (fn [a] [a (run-computer-until-first-output (assoc-in corrupted-computer [:cpu :a] a))]))
;     (filter (fn [[a c]] (= (first (:output c)) (first (:memory corrupted-computer)))))
;     (map (fn [[a c]] [a (run-computer-until-first-output c)]))
;     (filter (fn [[a c]] (= (second (:output c)) (second (:memory corrupted-computer)))))
;     (map (fn [[a c]] [a (run-computer-until-first-output c)]))
;     (filter (fn [[a c]] (= (first (drop 2 (:output c))) (first (drop 2 (:memory corrupted-computer))))))
;     (keep (fn [[a c]] (let [end-computer (run-computer c)]
;                          (when (= (:output end-computer)
;                                   (:memory corrupted-computer))
;                            (println "result is !!! " a)
;                            [a end-computer]))))))

(def start (power-of-2 (bigint 48)))

; (def computer (loop [candidate-a (dec (power-of-2 (bigint 48)))]
;                 (when (= (mod candidate-a 10000) 0)
;                   (println "jobber på kandidat " candidate-a)
;                   (println "siden start        " (- candidate-a start))
;                   (println "lengde på cache    " (count @previous-state))
;                   (println "lru queue size     " (count @recently-used) (count (set @recently-used)))
;                   (println "antall cache hits  " @number-of-cache-hits)
;                   (println "antall cache misses" @number-of-cache-misses)
;                   (println "totalt:            " (+ @number-of-cache-hits @number-of-cache-misses)))
;                 (let [end-computer (run-computer (assoc-in corrupted-computer [:cpu :a] candidate-a))]
;                   (assert false)
;                   (if (= (:output end-computer)
;                          (:memory corrupted-computer))
;                       (do
;                         (println "result is " candidate-a)
;                         end-computer)
;                       (recur (inc candidate-a))))))
; computer

; (->>
;   computer
;   :output
;   (join ","))



(deftest test-cases-from-part-1
  (is (= 1 (:b (:cpu (do (reset! previous-state {}) (run-computer {:cpu {:a 0 :b 0 :c 9 :instruction-pointer 0}
                                                                   :memory [2 6]
                                                                   :output []}))))))
  (is (= [0 1 2] (:output (do (reset! previous-state {}) (run-computer {:cpu {:a 10 :b 0 :c 0 :instruction-pointer 0}
                                                                        :memory [5 0 5 1 5 4]
                                                                        :output []})))))
  (is (= [4 2 5 6 7 7 7 7 3 1 0] (:output (do (reset! previous-state {}) (run-computer {:cpu {:a 2024 :b 0 :c 0 :instruction-pointer 0}
                                                                                        :memory [0 1 5 4 3 0]
                                                                                        :output []})))))
  (is (= 26 (:b (:cpu (do (reset! previous-state {}) (run-computer {:cpu {:a 0 :b 29 :c 0 :instruction-pointer 0}
                                                                    :memory [1 7]
                                                                    :output []}))))))
  (is (= 44354 (:b (:cpu (do (reset! previous-state {}) (run-computer {:cpu {:a 0 :b 2024 :c 43690 :instruction-pointer 0}
                                                                       :memory [4 0]
                                                                       :output []})))))))
  ; (is (= [4 6 3 5 6 3 5 2 1 0] (:output (run-computer (parse-program (slurp "resources/test/day-17-input.txt")))))))
