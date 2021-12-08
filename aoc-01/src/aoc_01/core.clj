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
  ;; Only works on horizontal-or-vertical? lines
  (if (= (:x point) (:x (:start line)))
    (or (<= (:y (:start line)) (:y point) (:y (:end line)))
        (<= (:y (:end line)) (:y point) (:y (:start line))))
    (if (= (:y point) (:y (:start line)))
      (or (<= (:x (:start line)) (:x point) (:x (:end line)))
          (<= (:x (:end line)) (:x point) (:x (:start line))))
      false)))

(def line-segments
  (->> input-file
       (slurp)
       (str/split-lines)
       (map #(str/split % #" "))
       (map (fn [coords] 
              {:start (parse-coorindate (first coords))
               :end (parse-coorindate (last coords))}))
       (filter horizontal-or-veritcal?)))

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
     (<= 2 (count (filter
                   (fn [line] 
                     (on-line? point line)) 
                   line-segments)))) 
   cartesian-plane))

(count overlapping-points)