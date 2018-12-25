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
             (function 1 2 3))))))
