(ns aoc-01.core
  (:gen-class)
  (:require [clojure.string :as str]))

;; (def input-file "./data/example.txt")
(def input-file "./data/input.txt")

(def data
  (->> input-file
       (slurp)
       (str/split-lines)))

(def drawn-numbers
  (map #(Integer/parseInt %) 
       (-> data 
           (first)  
           (str/split #","))))

(defn transpose [list-of-lists]
  (loop [old-matrix list-of-lists
         new-matrix []]
    (if (every? empty? old-matrix)
      new-matrix
      (recur
        (map rest old-matrix)       
        (conj new-matrix (map first old-matrix))))))

(def boards 
  (->> data
       (rest)
       (remove empty?)
       (partition 5)  
       (map 
        (fn [board]
          (map 
           (fn [row] 
             (map (fn [n] {:drawn false
                           :number (Integer/parseInt n)}) 
                  (remove empty? (str/split row #" "))))
           board)))))

(defn draw-number [n board]
  (map
   (fn [row]
     (map 
      (fn [cell]
        {:drawn (or (cell :drawn) (= n (cell :number)))
         :number (cell :number)})
      row))      
   board))   

(defn bingo? [row]
  (every? :drawn row))

(defn winner? [board]
  (or 
   (some bingo? board)   
   (some bingo?(transpose board))))

(defn find-first [f seq]
  (first (filter f seq)))

(def winner 
  (loop [active-boards boards
         numbers-to-draw drawn-numbers]
    (if (empty? numbers-to-draw)
      {:winner nil
       :winning-number nil}
      (let [drawn-number (first numbers-to-draw)
            rest-of-numbers (rest numbers-to-draw)
            updated-boards (map #(draw-number drawn-number %) active-boards)
            current-winner (find-first winner? updated-boards)]
        (print drawn-number "\n")
        (if (not (= nil current-winner))
          {:winner current-winner
           :winning-number drawn-number}
          (recur
           updated-boards       
           rest-of-numbers))))))

(* (winner :winning-number)
   (->> winner
     (:winner)
     (flatten)
     (remove :drawn)
     (map :number)
     (reduce +)))