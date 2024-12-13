(ns rest-of-advent-24.day-13-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [rest-of-advent-24.day-13 :refer [create-cases parse-case parse-literal]]))

(def testinput
  "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

(deftest oooops
  (testing "test create cases"
    (is (= 4 (count (create-cases testinput))))
    (is (= ["test" "hei"] (parse-literal "test" "testhei")))
    (is (= nil (parse-literal "test" "tesXhei")))))

(->>
  (create-cases testinput)
  (map parse-case)
  (map first))
