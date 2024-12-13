(ns rest-of-advent-24.day-12-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [rest-of-advent-24.day-12 :refer [calc-area]]))

(deftest lol
  (testing "calcs"
    (is (= 7 (calc-area 1)))))
