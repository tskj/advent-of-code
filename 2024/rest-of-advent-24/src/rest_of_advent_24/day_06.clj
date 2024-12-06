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

; (defn update-state-by-walking [m]
;   (let [guard-pos (get-guard-coords m)]
;     (if (nil? guard-pos)
;         m
;         (let [guard-dir (get-guard-direction m guard-pos)
;               new-potential-pos (add guard-pos guard-dir)]
;           (if (is-off-map? m new-potential-pos)
;             (-> m
;                 (write guard-pos \X))
;             (if (is-blocked? m new-potential-pos)
;               (let [new-position (add guard-pos (rotate-dir-clockwise guard-dir))]
;                 (-> m
;                     (write new-position (rotate-guard-clockwise (get-in m guard-pos)))
;                     (write guard-pos \X)))
;               (-> m
;                   (write new-potential-pos (get-in m guard-pos))
;                   (write guard-pos \X))))))))

(def initial-guard-pos (get-guard-coords m))
(def initial-guard-char (get-in m initial-guard-pos))

(defn update-state [m char-pos]
  (blk
    (const-try [char pos] char-pos)
    (const dir (get-guard-direction char))
    (const new-potential-pos (add pos dir))
    (if (is-off-map? new-potential-pos) (return nil))
    (if (is-blocked? m new-potential-pos)
      (let [new-pos (add pos (rotate-dir-clockwise dir))
            new-char (rotate-guard-clockwise char)]
        [new-char new-pos])
      [char new-potential-pos])))

(def init [initial-guard-char initial-guard-pos])

(def poss (atom []))
(boop [char-pos init] (some? char-pos) ((update-state m char-pos))
  (blk
    (const [char pos] char-pos)
    (swap! poss conj pos)
    pos))

(time
  (count (set @poss)))

; (time
;   (->>
;     (boop [m m prev-m nil] (not= m prev-m) ((update-state-by-walking m) m)
;           m)
;     (apply concat)
;     (filter (fn [x] (= x \X)))
;     (count)))
