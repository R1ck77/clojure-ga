(ns clojure-ga.fitness-proportionate-selection-test
  (require [clojure.test :refer :all]
           [clojure-ga.fitness-proportionate-selection :as selection]))

(defn- create-iterator
  "Returns a function that iterates over the elements and then returns identically nil"
  [elements]
  (let [remaining (atom elements)]
    (fn []
      (let [next (first @remaining)]
        (swap! remaining rest)
        next))))

(defn- selection-to-random [total-elements index]
  (double
   (+ (/ index total-elements)
      (/ 1 (* 2 total-elements)))))

(defn- create-fake-random [n-elements selections]
  (create-iterator (map #(selection-to-random n-elements %) selections)))

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
  (testing "general case"
    (is (= [:b :a :e :e :c]
           (selection/select 4 [:a :b :c :d :e]
                             (fn [x] (get {:a 1, :b 2, :c 3, :d 4, :e 5} x))
                             (create-fake-random 15 [14 15 4 2 12])))))) 
