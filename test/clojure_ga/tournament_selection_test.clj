(ns clojure-ga.tournament-selection-test
  (:require [clojure.test :refer :all]
            [clojure-ga.utils :as utils]
            [clojure-ga.selector :as selector]
            [clojure-ga.tournament-selection :as ts]))

(deftest tournment-selector-constructor-test
  (testing "sunny day"
    (let [correct-instance? (instance? clojure_ga.tournament_selection.TournmentSelector (ts/create-selector 1 (fn [_] 12) rand-nth))]
      (is correct-instance?)))
  (testing "fitness function not invoked during construction"
    (is (not (nil? (ts/create-selector 1 (fn [_] (assert false)) rand-nth)))))
  (testing "throws if rank < 1"
    (is (thrown? IllegalArgumentException (ts/create-selector 0 (fn [_] 12))))
    (is (thrown? IllegalArgumentException (ts/create-selector -1 (fn [_] 12)))))
  (testing "arguments assigned correctly"
    (let [selector (ts/create-selector 12 identity rand-nth)]
      (is (= 12 (get selector :rank)))
      (is (= identity (get selector :fitness-function))))))

(defn- mock-rand-nth
  "Like rand-nth, but the game is rigged"
  [& indices]
  (let [next-picks (atom indices)]
    (fn [coll]
      (let [current (first @next-picks)
            next (swap! next-picks rest)]
        (nth coll current)))))

(defmacro repeat-testXY [inputs ranks]
  (conj (for [input inputs
              rank ranks]    
          `(is (= (count ~input)
                  (count (selector/select (ts/create-selector ~rank identity rand-nth)
                                          ~input)))))
        'do))

(deftest selector-test
  (comment (testing "empty population → empty selection"
     (is (= [] (selector/select (ts/create-selector 12 identity rand-nth) []))))
   (testing "single element in the population → same element out"
     (is (= [44] (selector/select (ts/create-selector 12 identity rand-nth) [44]))))
   (testing "n elements in → n elements out, for any valid rank"
     (repeat-test [[] [0] [0 1] [0 1 2] [0 1 2 3] [0 1 2 3 4]]
                  (range 1 10)))
   (testing "many elements, rank 1"
     (let [fake-rand-int (mock-rand-nth 0 1 3 0 4 3)]
       (is (= [0 1 3 0 4 3]
              (selector/select (ts/create-selector 1 identity fake-rand-int)
                               [0 1 2 3 4 5]))))))
  (testing "many elements, rank 2"
    (let [fake-rand-int (mock-rand-nth 0 1 3 0 4 3 2 2 0 3 2 4)]
      (is (= [1 3 4 2 3 4]
             (selector/select (ts/create-selector 2 identity fake-rand-int)
                              [0 1 2 3 4 5])))))
  (testing "in case of tie, the first biggest element in a group wins"
    (let [fake-rand-int (mock-rand-nth 0 1 2 3
                                       2 0 3 0
                                       0 2 0 3
                                       3 0 2 0)]
      (is (= [:b :c :a :d]
             (selector/select (ts/create-selector 4 {:a 1 :b 2 :c 1 :d 1} fake-rand-int)
                              [:a :b :c :d]))))))
