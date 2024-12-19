(ns rest-of-advent-24.day-18
  (:require
   [clojure.math :as math]
   [clojure.string :refer [split split-lines]]
   [rest-of-advent-24.utils.macros :refer [blk]]))

(def file
  (->> (slurp "resources/day-18-input.txt")
       (split-lines)))

(def number-of-bytes-to-simulate
  (parse-long (first file)))

(def dimensions
  (mapv parse-long (split (second file) #"x")))

(def width (inc (first dimensions)))
(def height (inc (second dimensions)))

(def target [(dec width) (dec height)])

(def incoming-bytes
  (->> file
       (rest)
       (rest)
       (take number-of-bytes-to-simulate)
       (mapv #(split % #","))
       (mapv #(mapv parse-long %))))

(def walls (set incoming-bytes))

(defn is-out-of-bounds? [[x y]]
  (or (< x 0)
      (< y 0)
      (>= x width)
      (>= y height)))

(defn empty-map []
  (transient (vec (repeat (* width height) \.))))

(defn render [mmm things pencil]
  (->> things
       (reduce (fn [map' [x y]] (assoc! map' (+ (* y width) x) pencil)) mmm)))

(defn draw [map*]
  (blk
    (.write *out* "\033[2J\033[H")
    (.flush *out*)
    (const output (StringBuilder. (* 4 width height)))
    (doseq [y (range height)]
      (doseq [x (range width)]
        (let [char (get map* (+ (* y height) x))]
          (cond
            (= char \S) (.append output "\uD83E\uDDA5")
            (= char \.) (.append output "  ")
            :else       (.append output (str " " char)))))
      (.append output "\n"))
    (.write *out* (.toString output))
    (.flush *out*)))


(def draw-calls (atom 0))
(defn draw-everything [sloth? wall?]
    (blk
      (swap! draw-calls inc)
      (.write *out* "\033[2J\033[H")
      (.flush *out*)
      (const output (StringBuilder. (* 4 width height)))
      (dotimes [y height]
        (dotimes [x width]
          (let [c [x y]]
            (cond
              (sloth? c) (.append output "\uD83E\uDDA5")
              (wall? c)  (.append output " #")
              :else      (.append output "  "))))
        (.append output "\n"))
      (.write *out* (.toString output))
      (.flush *out*)))


(defn h [n]
  (let [[x y] n
        [x* y*] target]
    ; (+ (- x* x)
    ;    (- y* y))))
    (math/sqrt (+ (* (- x* x) (- x* x))
                  (* (- y* y) (- y* y))))))

(defn advance-sloth-up [sloth]
  (-> sloth
      (update-in [:p 1] dec)
      (update-in [:g] inc)
      (update-in [:path] conj (:p sloth))))

(defn advance-sloth-right [sloth]
  (-> sloth
      (update-in [:p 0] inc)
      (update-in [:g] inc)
      (update-in [:path] conj (:p sloth))))

(defn advance-sloth-down [sloth]
  (-> sloth
      (update-in [:p 1] inc)
      (update-in [:g] inc)
      (update-in [:path] conj (:p sloth))))

(defn advance-sloth-left [sloth]
  (-> sloth
      (update-in [:p 0] dec)
      (update-in [:g] inc)
      (update-in [:path] conj (:p sloth))))

(defn n-for-sloth [sloth]
  (->>
    [advance-sloth-up advance-sloth-right advance-sloth-down advance-sloth-left]
    (mapv (fn [f] (f sloth)))
    (remove (fn [sloth] (is-out-of-bounds? (:p sloth))))))

(defn f [sloth]
  (+ (:g sloth) (h (:p sloth))))

(def visited-nodes (atom #{[0 0]}))

(defn is-finished? [sloth]
  (when (= target (:p sloth))
    sloth))

(def wait (atom 10))

(defn run-loop []
  (time
    (loop [sloths [{:g 0 :p [0 0] :path []}]
           i 0]
      ; (-> (empty-map)
      ;     (render (map :p sloths) \S)
      ;     (render walls \#)
      ;     (persistent!)
      ;     (draw))
      (when (= (mod i 2) 0)
        (draw-everything (set (map :p sloths)) walls))
      (if-let [finish (some is-finished? sloths)] [i finish]
        (let [p-sloth (apply min-key f sloths)
              new-sloths (->> (n-for-sloth p-sloth)
                              (remove (fn [sloth] (walls (:p sloth))))
                              (remove (fn [sloth] (@visited-nodes (:p sloth)))))]
          (swap! visited-nodes into (map :p new-sloths))
          (recur (->> sloths
                      (remove (fn [sloth] (= (:p p-sloth) (:p sloth))))
                      (concat new-sloths))
                 (inc i)))))))
