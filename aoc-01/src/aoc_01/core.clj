(ns aoc-01.core
  (:gen-class)
  (:require [clojure.string :as str]))

;; (def input-file "./data/example.txt")
(def input-file "./data/input.txt")

(defn parse-coorindate [coord]
  (let [[x y] (str/split coord #",")]
    {:x (Integer/parseInt x)
     :y (Integer/parseInt y)}))

(defn horizontal-or-veritcal? [segment]
  (or (= (:x (:start segment))
         (:x (:end segment)))
      (= (:y (:start segment))
         (:y (:end segment)))))

(defn on-line? [point line]
  (if (horizontal-or-veritcal? line) 
    (if (= (:x point) (:x (:start line)))
      (or (<= (:y (:start line)) (:y point) (:y (:end line)))
          (<= (:y (:end line)) (:y point) (:y (:start line))))
      (if (= (:y point) (:y (:start line)))
        (or (<= (:x (:start line)) (:x point) (:x (:end line)))
            (<= (:x (:end line)) (:x point) (:x (:start line))))
        false))
    (let [line-start-x (:x (:start line)) 
          line-end-x   (:x (:end line))
          line-start-y (:y (:start line))
          line-end-y   (:y (:end line))]
      (and 
       (= (Math/abs (- line-end-y line-start-y))
          (Math/abs (- line-end-x line-start-x)))
       (and (or 
             (<= line-start-x (:x point) line-end-x)
             (>= line-start-x (:x point) line-end-x))
            (or
             (<= line-start-y (:y point) line-end-y)
             (>= line-start-y (:y point) line-end-y)))
       (= (Math/abs (- (:y point) line-start-y))
          (Math/abs (- (:x point) line-start-x)))))))

(def line-segments
  (->> input-file
       (slurp)
       (str/split-lines)
       (map #(str/split % #" "))
       (map (fn [coords] 
              {:start (parse-coorindate (first coords))
               :end (parse-coorindate (last coords))}))))

(def corners
  (let [all-points (concat 
                    (map :start line-segments) 
                    (map :end line-segments))
        start-point {:x (apply min (map :x all-points))
                     :y (apply min (map :y all-points))}
        end-point {:x (apply max (map :x all-points))
                   :y (apply max (map :y all-points))}]
    {:start start-point :end end-point}))

(def cartesian-plane
  (->> (range (:x (:start corners)) (+ 1 (:x (:end corners))))
       (mapcat 
        (fn [x]
          (->> (range (:y (:start corners)) (+ 1 (:y (:end corners))))
               (map 
                (fn [y]
                  {:x x :y y})))))))

(def overlapping-points
  (filter 
   (fn [point] 
     (print point "\n")
     (<= 2 (count (take 2 (filter
                           (fn [line]
                             (on-line? point line))
                           line-segments))))) 
   cartesian-plane))

(count overlapping-points)
