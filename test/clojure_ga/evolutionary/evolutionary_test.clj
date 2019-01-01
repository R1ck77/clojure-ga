(ns clojure-ga.evolutionary.evolutionary-test
  (:require [clojure.test :refer :all]
            [clojure-ga.evolutionary.evolutionary :as evo]))


(deftest expression-to-function-test
  (testing "expression without variables"
    (let [function (evo/expression-to-function '(√ (+ (sin 12) 6)))
          expected-function #(Math/sqrt (+ (Math/sin 12) 6))]
      (is (= (expected-function)
             (function)))))
  (testing "single variable expression"
    (let [function (evo/expression-to-function '(* :x 2) :x)
          expected-function #(* % 2)]
      (is (= (expected-function 3)
             (function 3)))
      (is (= (expected-function 0)
             (function 0)))      
      (is (= (expected-function -10)
             (function -10)))))
  (testing "multiple variables galore"
    (let [function (evo/expression-to-function '(√ (+ (* :x :x) (* :y :y) (* :z :z))) :x :y :z)
          expected-function #(Math/sqrt (+ (* % %) (* %2 %2) (* %3 %3)))]
      (is (= (expected-function 1 2 3)
             (function 1 2 3)))))
  (testing "catches exceptions"
    (let [function (evo/expression-to-function '(/ 1 :x) :x)
          expected-function #(/ 1 %)]
      (is (= (expected-function 10)
             (function 10)))
      (is (Double/isNaN (function 0))))))

(deftest evaluate-chromosome-test
  (testing "evaluate constant data with no error"
    (is (= 0.0 (evo/evaluate-chromosome 10
                              [[10 14 23] [10 0 23] [10 -100 -100]]
                              [:x :y])))
    (is (= 0.0 (evo/evaluate-chromosome '(+ 10 (+ (* :x 0) (* :y 0)))
                              [[10 14 23] [10 0 23] [10 -100 -100]]
                              [:x :y]))))
  (testing "evaluate constant data"
    (is (= (double (- (+ 47 33 190)))
           (evo/evaluate-chromosome '(+ 10 (+ :x :y))
                                    [[0 14 23] [0 0 23] [0 -100 -100]]
                                    [:x :y]))))
  (testing "linear interpolation, no error"
    (is (= 0.0
           (evo/evaluate-chromosome '(+ 10 (* 3 :x))
                                    [[10 0] [13 1] [310 100]]
                                    [:x]))))
  (testing "linear interpolation, max error"
    (is (= (double (- (+ 5 5 5)))
           (evo/evaluate-chromosome '(+ 15 (* 3 :x))
                                    [[10 0] [13 1] [310 100]]
                                    [:x])))))

(deftest test-variables-from-points
  (testing "0 variables case"
    (is (= [] (evo/variables-from-points [[1] [2] [3]]))))
  (testing "1 variable case"
    (is (= [:0] (evo/variables-from-points [[1 1] [2 2] [3 3]]))))
  (testing "general case"
    (is (= [:0 :1 :2 :3]
           (evo/variables-from-points [[0 11 22 33 44] [23 23 34 12 98]]))))  )
