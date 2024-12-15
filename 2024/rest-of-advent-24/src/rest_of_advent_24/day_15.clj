(ns rest-of-advent-24.day-15
  (:require
   [clojure.string :refer [split split-lines]]
   [rest-of-advent-24.utils.macros :refer [blk]]))

(def input
  (->> (slurp "resources/day-15-input.txt")
       (#(split % #"\n\n"))))

(def warehouse
  (let [[m _] input]
    (->> m
         (split-lines)
         (map (fn [line]
                (->> line
                     (mapcat
                       (fn [tile]
                         (case tile
                          \# [\# \#]
                          \O [\[ \]]
                          \. [\. \.]
                          \@ [\@ \.]))))))
         (mapv vec))))

(def width (count (first warehouse)))
(def height (count warehouse))

(def robot-movements
  (let [[_ r] input]
    (->> r
         (split-lines)
         (mapv vec)
         (apply concat))))

(defn find-robot [m]
  (let [robot (atom nil)]
    (doseq [y (range height)]
      (doseq [x (range width)]
        (when (= \@ (get-in m [y x]))
          (reset! robot [y x]))))
    (assert (some? @robot))
    @robot))

(defn is-wall? [m p]
  (= \# (get-in m p)))

(defn is-box? [m p]
  (or (= \[ (get-in m p))
      (= \] (get-in m p))))

(defn is-box-left? [m p]
  (= \[ (get-in m p)))

(defn is-empty? [m p]
  (= \. (get-in m p)))

(defn is-robot? [m p]
  (= \@ (get-in m p)))

(defn is-on-map? [m p]
  (let [[y x] p]
    (and (< y height)
         (< x width)
         (>= y 0)
         (>= x 0))))

(def up     [-1 0])
(def down   [1 0])
(def right  [0 1])
(def left   [0 -1])

(defn rev-dir [[y x]]
  [(* -1 y) (* -1 x)])

(defn add [[a b] [c d]]
  [(+ a c) (+ b d)])

(defn get-full-box-coords [m p]
  (assert (is-box? m p))
  (case (get-in m p)
    \[ (let [p* (add p right)
             b* (get-in m p*)]
         (assert (= b* \]))
         [p p*])
    \] (let [p* (add p left)
             b* (get-in m p*)]
         (assert (= b* \[))
         [p* p])))

(defn swp [m a b]
  (let [a' (get-in m a)
        b' (get-in m b)]
    (-> m
        (assoc-in a b')
        (assoc-in b a'))))

(defn step [m s]
  (let [r (find-robot m)
        d (case s
            \^ up
            \> right
            \v down
            \< left)
        potential (add r d)]
    (blk
      (if (is-empty? m potential) (return (swp m r potential)))
      (if (is-wall? m potential) (return m))
      (assert (is-box? m potential))

      (const flat-box-coords (atom #{}))

      (const end-coords
             (if (or (= d left)
                     (= d right))
               (loop [new-p potential]
                 (if (is-box? m new-p)
                     (do
                       (swap! flat-box-coords conj new-p)
                       (recur (add new-p d)))
                     (if (or (is-empty? m new-p) (is-wall? m new-p))
                         [new-p]
                         (assert false "reached end of boxes but not wall or empty somehow"))))
               (loop [new-ps [potential]]
                 (let [expand-boxes (fn [p] (if (is-box? m p)
                                                (get-full-box-coords m p)
                                                [p]))
                       new-ps (mapcat expand-boxes new-ps)]
                   (if (->> new-ps (some #(is-box? m %)))
                       (let [end-coords (->> new-ps (filter (fn [p] (or (is-empty? m p) (is-wall? m p)))))
                             box-coords (->> new-ps (filter #(is-box? m %)))]
                         (swap! flat-box-coords into box-coords)
                         (assert (not (some #(is-robot? m %) new-ps)))
                         (assert (not (empty? box-coords)))
                         (recur (concat end-coords (map #(add % d) box-coords))))
                       new-ps)))))
      (if (some #(is-wall? m %) end-coords) (return m))
      (assert (every? #(is-empty? m %) end-coords))
      (assert (every? #(is-box? m %) @flat-box-coords))

      (const unique-box-structure (set (map #(get-full-box-coords m %) @flat-box-coords)))
      (doseq [[p1 p2] unique-box-structure]
        (assert (is-box? m p1))
        (assert (is-box? m p2)))

      (const map-without-boxes (->> unique-box-structure
                                    (reduce (fn [m* [p1 p2]]
                                              (-> m*
                                                  (assoc-in p1 \.)
                                                  (assoc-in p2 \.)))
                                            m)))
      (const new-box-positions (->> unique-box-structure
                                    (map (fn [[p1 p2]] [(add p1 d)
                                                        (add p2 d)]))))

      (const map-with-new-box-positions (->> new-box-positions
                                             (reduce (fn [m* [p1 p2]]
                                                       (-> m*
                                                           (assoc-in p1 \[)
                                                           (assoc-in p2 \])))
                                                     map-without-boxes)))

      (const final-map (swp map-with-new-box-positions r potential))
      final-map)))

(def final-warehouse
  (->> robot-movements
       (reduce step warehouse)))

(defn gps-of-coord [[y x]]
  (+ (* y 100) x))

(let [box-coords (atom #{})]
  (doseq [y (range height)]
    (doseq [x (range width)]
      (when (is-box-left? final-warehouse [y x])
        (swap! box-coords conj [y x]))))
  (->> @box-coords
       (map gps-of-coord)
       (reduce +)))
