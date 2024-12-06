(ns rest-of-advent-24.day-05
  (:require
   [clojure.string :refer [split split-lines]]))

(def page
  (->> (slurp "resources/day-05-input.txt")
       (split-lines)))

(def rules
  (->>
    page
    (partition-by empty?)
    (first)
    (map #(split % #"\|"))
    (map (fn [x] (map parse-long x)))))

(def updates
  (->>
    page
    (partition-by empty?)
    (last)
    (map #(split % #","))
    (map (fn [x] (map parse-long x)))))

(defn includes [x coll]
  (some #(= % x) coll))

(defn rule-is-not-violated? [rule update]
  (let [numbers-in-update (->> update (filter (fn [x] (includes x rule))))]
    (assert (= (count numbers-in-update)
               (count rule)))
    (= rule numbers-in-update)))

(defn is-acceptable? [update]
  (let [applicable-rules (->> rules (filter (fn [[a b]] (and (includes a update)
                                                             (includes b update)))))]
    (every? #(rule-is-not-violated? % update) applicable-rules)))

(defn swap [coll i-a i-b]
  (let [a (get coll i-a)
        b (get coll i-b)]
    (-> coll
        (assoc i-a b)
        (assoc i-b a))))

(defn find-idx [n coll]
  (let [indices (->> coll
                 (map-indexed (fn [idx x] (when (= x n) idx)))
                 (filter some?))]
    (assert (= 1 (count indices)))
    (first indices)))

(defn fix-update [update]
  (let [applicable-rules (->> rules (filter (fn [[a b]] (and (includes a update)
                                                             (includes b update)))))
        result (->> [update applicable-rules]
                    (apply reduce (fn [u r]
                                   (if (rule-is-not-violated? r u)
                                     u
                                     (swap (vec u)
                                           (find-idx (first r) u)
                                           (find-idx (second r) u))))))]
    (if (not (is-acceptable? result))
      (fix-update result)
      result)))

(defn middle-number [update]
  (if (<= (count update) 1)
    (do (assert (= (count update) 1))
        (first update))
    (middle-number (->> update
                        (rest)
                        (reverse)
                        (rest)
                        (reverse)))))

(->> updates
     (filter (comp not is-acceptable?))
     (map fix-update)
     (map middle-number)
     (reduce +))
