(ns rest-of-advent-24.day-15
  (:require
   [clojure.string :refer [split split-lines]]
   [rest-of-advent-24.utils.macros :refer [blk]]))

(def input
  (->> (slurp "resources/test/day-15-input.txt")
       (#(split % #"\n\n"))))

(def warehouse
  (let [[m _] input]
    (->> m
         (split-lines)
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
        (when (= \@ (get-in warehouse [y x]))
          (reset! robot [y x]))))
    (assert (some? @robot))
    @robot))

(defn is-wall? [m p]
  (= \# (get-in m p)))

(defn is-box? [m p]
  (= \O (get-in m p)))

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

(defn add [[a b] [c d]]
  [(+ a c) (+ b d)])

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

      (const end-coord
             (loop [new-p potential]
               (if (is-box? m new-p)
                   (recur (add new-p dir))
                   (if (or (is-empty? m new-p) (is-wall? m new-p))
                       new-p
                       (assert false "reached end of boxes but not wall or empty somehow")))))
      (if (is-wall? m end-coord) (return m)) 

      (assert (is-empty? m end-coord))
      (loop [m' m])))) 
              
