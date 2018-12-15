(ns clojure-ga.fitness-proportionate-selection-test
  (:require [clojure.test :refer :all]
            [clojure-ga.utils :as utils]
           [clojure-ga.fitness-proportionate-selection :as selection]))

(defn- selection-to-random [total-elements index]
  (double
   (/ index total-elements)))

(defn- create-fake-random [n-elements selections]
  (utils/create-iterator (map #(selection-to-random n-elements %) selections)))

(defn- score-keyword [keyword]
  (get {:a 1, :b 2, :c 3, :d 4, :e 5} keyword))

(deftest test-select
  (testing "select from an empty population returns an empty list"
    (is (= [] (selection/select 10 [] (fn [x] 12) rand))))
  (testing "selecting from a single value population returns N times the same instance"
    (is (= [:a :a :a :a]  (selection/select 4 [:a] (fn [x] 12) rand))))
  (testing "selection from a zero fitness sum of chromosomes returns the last value repeatedly"
    (is (= [:e :e :e :e]
           (selection/select 4 [:a :b :c :d :e]
                             (fn [x] 0)
                             (create-fake-random 5 [0 2 4 1])))))
  (testing "small case, with duplicates"
    (is (= [:a :b :b :a]     ;;; expected: [[b 2] [a 3]]
           (selection/select 4 [:a :b]
                             score-keyword
                             (create-fake-random 3 [2.1 0 1.999999 2.9])))))
  (testing "general cases"
    (is (= [:a :b :c :d :e]
           (selection/select 5 
                             [:a :b :c :d :e] ;;; -> [[:e 5] [:d 9] [:c 12] [:b 14] [:a 15]]
                             score-keyword
                             (create-fake-random 15 [14.5 12.5 9.5 5.5 4.5]))))
   
    (is (= [:a :a :e :e :c]
           (selection/select 5
                             [:a :b :c :d :e] ;;; -> [[:e 5] [:d 9] [:c 12] [:b 14] [:a 15]]
                             score-keyword
                             (create-fake-random 15 [14 20 4 2 10])))))) 


