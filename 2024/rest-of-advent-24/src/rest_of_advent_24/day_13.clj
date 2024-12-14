(ns rest-of-advent-24.day-13
  (:require
   [clojure.string :refer [split-lines] :as s]
   [clojure.test :refer [deftest is testing]]
   [rest-of-advent-24.utils.macros :refer [blk]]))

(defn create-cases [input]
  (->> input
       (split-lines)
       (partition-by #(= % ""))
       (remove #(= % [""]))))

(def input
  (->> (slurp "resources/day-13-input.txt")
       (create-cases)))

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

(defn parse-case [case]
  (assert (->> (count case)
               (= 3)))
  (blk
    (const [a b p] case)

    (const-try [_ s]      (parse-literal "Button A: X+" a))
    (const-try [digits s] (parse-n-digit-int s))
    (const-try a-x        (parse-long digits))
    (const-try [_ s]      (parse-literal ", Y+" s))
    (const-try [digits s] (parse-n-digit-int s))
    (const-try a-y        (parse-long digits))

    (const-try [_ s]      (parse-literal "Button B: X+" b))
    (const-try [digits s] (parse-n-digit-int s))
    (const-try b-x        (parse-long digits))
    (const-try [_ s]      (parse-literal ", Y+" s))
    (const-try [digits s] (parse-n-digit-int s))
    (const-try b-y        (parse-long digits))

    (const-try [_ s]      (parse-literal "Prize: X=" p))
    (const-try [digits s] (parse-n-digit-int s))
    (const-try p-x        (parse-long digits))
    (const-try [_ s]      (parse-literal ", Y=" s))
    (const-try [digits s] (parse-n-digit-int s))
    (const-try p-y        (parse-long digits))

    [{:ax a-x :bx b-x :ay a-y :by b-y :px (+ p-x 10000000000000) :py (+ p-y 10000000000000)} s]))

(def all-case-equations
  (->> input
       (map parse-case)
       (keep identity)
       (map first)))

(assert (= (count all-case-equations) (count input)))

(defn solve-equation [{:keys [ax bx ay by px py]}]
  (blk
    (const det (- (* ax by)
                  (* bx ay)))
    (if (= det 0) (return nil))

    (const a' (- (* by px)
                 (* bx py)))
    (const b' (- (* ax py)
                 (* ay px)))

    [(/ a' det) (/ b' det)]))

(->> all-case-equations
     (map solve-equation)
     (filter (fn [[a b]] (and (int? a) (int? b))))
     (map (fn [[a b]] (+ (* 3 a) b)))
     (reduce +))

