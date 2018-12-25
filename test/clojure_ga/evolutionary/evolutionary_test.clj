(ns clojure-ga.evolutionary.evolutionary-test
  (:require [clojure.test :refer :all]
            [clojure-ga.evolutionary.evolutionary :as evo]))


(deftest expression-to-function-test
  (testing "expression without variables"
    (let [function (evo/expression-to-function '(√ (+ (sin 12) 6)))
          expected-function #(Math/sqrt (+ (Math/sin 12) 6))]
      (is (= (expected-function)
             ((eval function)))))))
