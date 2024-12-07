(ns rest-of-advent-24.day-06
  (:require
   [clojure.string :refer [split-lines]]
   [rest-of-advent-24.utils.elves :refer [includes]]
   [rest-of-advent-24.utils.macros :refer [blk boop]]))

(def m
  (->> (slurp "resources/day-06-input.txt")
       (split-lines)
       (map vec)
       (vec)))

(def guard-chars [\^ \> \V \<])

(def y-size (count m))
(def x-size (count (first m)))

(defn get-guard-coords [m]
  (let [pos (atom nil)]
    (assert (->> m (every? (fn [row] (= (count row) x-size)))) "map isn't rectangular")
    (boop [y 0] (< y y-size) ((inc y))
          (boop [x 0] (< x x-size) ((inc x))
                (when (->> guard-chars (includes (get-in m [y x])))
                  (reset! pos [y x]))))
    @pos))

(defn get-guard-direction [guard-char]
  (assert (->> guard-chars (includes guard-char)) (str "found something else than a guard: " guard-char))
  (case (.indexOf guard-chars guard-char)
    0 [-1 0]
    1 [0 1]
    2 [1 0]
    3 [0 -1]))

(defn add [[y x] [dy dx]]
  [(+ y dy) (+ x dx)])

(defn rotate-dir-clockwise [[dy dx]]
  [dx (* -1 dy)])

(defn rotate-guard-clockwise [guard-char]
  (->> (.indexOf guard-chars guard-char)
       (inc)
       (#(mod % (count guard-chars)))
       (get guard-chars)))

(defn is-blocked? [m p]
  (= (get-in m p) \#))

(defn is-off-map? [[y x]]
  (cond
    (< y 0) true
    (< x 0) true
    (>= y y-size) true
    (>= x x-size) true
    :else false))

(defn write [m p c]
  (update-in m p (fn [_] c)))

(def initial-guard-pos (get-guard-coords m))
(def initial-guard-char (get-in m initial-guard-pos))

(defn update-state
  ([m char-pos]
   (blk
     (assert (vector? m))
     (assert (vector? (first m)))
     (assert (= (count char-pos) 2))

     (const-try [char pos] char-pos)
     (const dir (get-guard-direction char))
     (const new-potential-pos (add pos dir))

     (if (is-off-map? new-potential-pos) (return nil))

     (if (is-blocked? m new-potential-pos)
       (let [new-pos (add pos (rotate-dir-clockwise dir))
             new-char (rotate-guard-clockwise char)]
         [new-char new-pos])
       [char new-potential-pos])))

  ([m char-pos pound-pos]
   (blk
     (assert (vector? m))
     (assert (vector? (first m)))
     (assert (= (count char-pos) 2))

     (const-try [char pos] char-pos)
     (const dir (get-guard-direction char))
     (const new-potential-pos (add pos dir))

     (if (is-off-map? new-potential-pos) (return nil))

     (if (or (= new-potential-pos pound-pos) (is-blocked? m new-potential-pos))
       (let [new-pos (add pos (rotate-dir-clockwise dir))
             new-char (rotate-guard-clockwise char)]
         [new-char new-pos])
       [char new-potential-pos]))))

(def init [initial-guard-char initial-guard-pos])

(def char-poss (atom []))
(boop [char-pos init] (some? char-pos) ((update-state m char-pos))
  (blk
    (const [char pos] char-pos)
    (swap! char-poss conj [char pos])))

; (doseq [x
;         (->> @char-poss (reduce (fn [m [char pos]] (update-in m pos (fn [_] char))) m))]
;   (println (apply str x)))

(defn loops? [[char pos]]
  (let [pound-sign (add pos (get-guard-direction char))]
    (loop [char-poss []
           char-pos init
           counter 0]
      (when (= (mod counter 1000) 0) (println (str "count: " counter)))
      (blk
        (const new-char-pos (update-state m char-pos pound-sign))
        (if (nil? new-char-pos) nil)
        (if (contains? (set char-poss) new-char-pos)
          (do (assert (not= (get-in m pound-sign) \#) "it should not be possible to create loop by placing a pound on a pound")
              pound-sign))
        (recur (conj char-poss new-char-pos) new-char-pos (inc counter))))))

(->> @char-poss
     ; (filter (fn [[char pos]] (not= pos initial-guard-pos)))
     (map-indexed (fn [idx char-pos] (println (str "iteration number: " idx)) (loops? char-pos)))
     (filter some?)
     (filter #(not= initial-guard-pos %))
     (set)
     (count))

; (time
;   (->>
;     (boop [m m prev-m nil] (not= m prev-m) ((update-state-by-walking m) m)
;           m)
;     (apply concat)
;     (filter (fn [x] (= x \X)))
;     (count)))
