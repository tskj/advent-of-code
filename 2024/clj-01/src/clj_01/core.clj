(ns clj-01.core
  (:require
   [clojure.string :refer [split]]))

; (->>
;   (slurp "resources/input.txt")
;   (#(split % #"\n"))
;   (map (fn [x] (split x #"   ")))
;   (apply map vector)
;   (map #(map parse-long %))
;   (map sort)
;   (#(map vector (first %) (second %)))
;   (map (fn [[a b]] (Math/abs (- a b))))
;   (apply +))

(def inputs
  (->>
    (slurp "resources/input.txt")
    (#(split % #"\n"))
    (map (fn [x] (split x #"   ")))
    (apply map vector)
    (map #(map parse-long %))))

(defn how-many-times? [x xs]
  (->> xs
       (filter (fn [y] (= y x)))
       (count)))

(->> (first inputs)
  (map (fn [x] (* x (how-many-times? x (second inputs)))))
  (apply +))
