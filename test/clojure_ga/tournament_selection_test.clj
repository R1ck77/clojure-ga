(ns clojure-ga.tournament-selection-test
  (:require [clojure.test :refer :all]
            [clojure-ga.utils :as utils]
            [clojure-ga.selector :as selector]
            [clojure-ga.tournament-selection :as ts]))

(deftest tournment-selector-constructor-test
  (testing "sunny day"
    (let [correct-instance? (instance? clojure_ga.tournament_selection.TournmentSelector (ts/create-selector 1 (fn [_] 12)))]
      (is correct-instance?)))
  (testing "fitness function not invoked during construction"
    (is (not (nil? (ts/create-selector 1 (fn [_] (assert false)))))))
  (testing "throws if rank < 1"
    (is (thrown? IllegalArgumentException (ts/create-selector 0 (fn [_] 12))))
    (is (thrown? IllegalArgumentException (ts/create-selector -1 (fn [_] 12)))))
  (testing "arguments assigned correctly"
    (let [selector (ts/create-selector 12 identity)]
      (is (= 12 (get selector :rank)))
      (is (= identity (get selector :fitness-function))))))

(deftest selector-test
  (testing "empty population → empty selection"
    (is (= [] (selector/select (ts/create-selector 12 identity) [])))))
