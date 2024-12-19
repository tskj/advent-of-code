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

(def all-bytes
  (->> file
       (rest)
       (rest)
       (mapv #(split % #","))
       (mapv #(mapv parse-long %))))

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
      (const output (StringBuilder.))
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
    ; 0))

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


(defn is-finished? [sloth]
  (when (= target (:p sloth))
    sloth))


(defn dedup-overlapping-sloths [sloths]
  (let [dup-poss (->> (frequencies (map :p sloths))
                      (keep (fn [[k v]] (when (> v 1)
                                           k))))
        pos-best-map (->> dup-poss (map (fn [p] [p (->> sloths
                                                      (filter #(= (:p %) p))
                                                      (map :g)
                                                      (apply min))]))
                                   (into {}))]
    (->> sloths (remove (fn [sloth] (if-let [g (pos-best-map (:p sloth))]
                                          (not= (:g sloth) g)
                                          false)))
                (map (fn [sloth] [(select-keys sloth [:g :p]) sloth]))
                (into {})  ;; dedups equal g and p
                (map (fn [[k v]] v)))))


(def initial-sloth {:g 0 :p [0 0] :path []})

(defn run-A* [sloth walls]
  (let [visited-nodes (atom {[0 0] 0})]
    (loop [sloths [sloth]
           i 0]
      (if (empty? sloths) nil
        (do

          ; (let [sloth-ps   (map :p sloths)
          ;       set-sloths (set sloth-ps)]
          ;   (assert (= (count sloth-ps)
          ;              (count set-sloths)) "multiple sloths in same tile!!"))

          ; (when (= (mod i 1) 0)
          ;   (draw-everything (set (map :p sloths)) walls))

          (if-let [finish (some is-finished? sloths)] [i finish] ;; on your face
            (let [p-sloth (apply min-key f sloths)
                  new-sloths (->> (n-for-sloth p-sloth)
                                  (remove (fn [sloth] (walls (:p sloth))))
                                  (remove (fn [sloth] (if-let [node (@visited-nodes (:p sloth))]
                                                        (<= node (:g sloth))
                                                        false))))]
              (swap! visited-nodes into (map (fn [sloth] [(:p sloth) (:g sloth)]) new-sloths))
              (recur (->> sloths
                          (remove (fn [sloth] (= (:p p-sloth) (:p sloth))))
                          (concat new-sloths)
                          (dedup-overlapping-sloths))
                     (inc i)))))))))

(defn run-loop []
  (loop [i 1024]
    (let [bytes (take i all-bytes)
          walls (set bytes)
          r (run-A* initial-sloth walls)]
      (if-let [[_ sloth] r]
        (do
          (when (= (mod i 100) 0)
            (draw-everything (set (:path sloth)) (set bytes))
            (println "we are at iteration" i))
          (recur (inc i)))
        (println "finished with this byte:" (last bytes) " which was number:" i)))))
