(ns rest-of-advent-24.day-14
  (:require
   [clojure.string :refer [split-lines] :as s]
   [clojure.test :refer [deftest is]]
   [rest-of-advent-24.utils.macros :refer [blk]]))

(def input
  (->> (slurp "resources/day-14-input.txt")
       (split-lines)))

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

(defn parse-sign [s]
  (blk
    (const [sign s] (or (parse-literal "-" s)
                      ["+" s]))
    (case sign
      "-" [-1 s]
      "+" [1 s])))

(deftest sign-parse
  (is (= -1 (first (parse-sign "-2314"))))
  (is (= 1 (first (parse-sign "2314")))))

(defn parse-line [s]
  (blk
    (const-try [_ s]    (parse-literal "p=" s))
    (const     [spx s]  (parse-sign s))
    (const-try [px s]   (parse-n-digit-int s))
    (const-try [_ s]    (parse-literal "," s))
    (const     [spy s]  (parse-sign s))
    (const-try [py s]   (parse-n-digit-int s))

    (const-try [_ s]    (parse-literal " v=" s))
    (const     [svx s]  (parse-sign s))
    (const-try [vx s]   (parse-n-digit-int s))
    (const-try [_ s]    (parse-literal "," s))
    (const     [svy s]  (parse-sign s))
    (const-try [vy s]   (parse-n-digit-int s))

    [[[(* spx px) (* spy py)] [(* svx vx) (* svy vy)]] s]))

(deftest test-parse-line
  (is (= [[0 4] [3 -3]] (first (parse-line "p=0,4 v=3,-3")))))


(defn parse-file [file]
  (blk
    (assert (> (count file) 1))
    (const header (first file))
    (const positions-and-velocities (rest file))

    (const-try [_ s]      (parse-literal "width=" header))
    (const-try [width s]  (parse-n-digit-int s))
    (const-try [_ s]      (parse-literal " height=" s))
    (const-try [height s] (parse-n-digit-int s))

    (const data (->> (map parse-line positions-and-velocities)
                     (map first)))

    {:dimensions [width height]
     :data data}))

(defn step [[width height] [x y] [vx vy]]
  (let [new-x (+ x vx)
        new-y (+ y vy)]
    [(mod new-x width) (mod new-y height)]))

(defn simulate [width height]
  (fn [[pos vel]]
    (loop [iterations 1
           current-pos pos]
      (if (<= iterations 0)
        [current-pos vel]
        (recur (dec iterations)
               (step [width height] current-pos vel))))))

(def n (atom 0))

(defn print-robots [w h positions]
  (blk
    ; (const output (StringBuilder.))
    (doseq [y (range h)]
      (doseq [x (range w)]
        (let [number-here (transduce (keep (fn [r] (when (= r [x y]) 1))) + 0 positions)]
          (if (= number-here 0))))
            ; (.append output "   ")
            ; (.append output (str number-here)))))
      (.append output "\n"))
    (.write *out* "\033[H\033[2J")
    (.write *out* (str "number of steps: " @n "\n"))
    (.write *out* (.toString output))
    (.flush *out*)))

(def start (parse-file input))

(def positions (atom (:data start)))

(defn run []
  (blk
    (const {[width height] :dimensions} start)

    (const final-positions (map (simulate width height) @positions))
    (swap! n inc)
    (reset! positions final-positions)

    (print-robots width height (map first final-positions))))

(def wait (atom 10))

(defn run-loop []
  (future
    (try
      (while true
        (run))
      (catch InterruptedException e
        (println "Loop interrupted")))))
