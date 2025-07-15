#!/usr/bin/env bb
(require '[clojure.string :as str])

(def input
  (->> "./day-10-input-part-1.txt"
       (slurp)
       (str/split-lines)
       (map parse-long)
       sort))

(def input*
  (->> input
       (concat [0])))

(defn explode [list]
  (if (empty? list)
    []
    (concat [list]
            (explode (rest list)))))

(explode [1 2 3])
(explode [5])
(explode [])

(def how-many?
  (memoize
   (fn [list]
     (if (empty? list)
       1

       (let [start (first list)
             stuff (explode (rest list))

             legal-gap?  (fn [rest-list]
                           (if (empty? rest-list)
                             true
                             (<= (- (first rest-list) start) 3)))]

         (->> stuff
              (filter legal-gap?)
              ((fn [x] (if (empty? x) [[]] x)))
              (map how-many?)
              (reduce +)))))))

(time (how-many? input*))

