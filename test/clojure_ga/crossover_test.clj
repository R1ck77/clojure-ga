(ns clojure-ga.crossover-test
  (:require [clojure.test :refer :all]
            [clojure-ga.utils :as utils]
            [clojure-ga.crossover :as crossover]))

(defn create-throwing-crossover []
  (crossover/->SimpleCrossover (fn [_ _] (is false "operator invoked but not expected"))))

(deftest not-enough-elements
  (testing "doesn't invoke the cross operator on an empty population"
    (crossover/combine (create-throwing-crossover) [])
    (is true))
  (testing "returns an empty vector on an empty population"
    (is (= []
           (crossover/combine (create-throwing-crossover) []))))  
  (testing "doesn't invoke the cross operator on a population of one element"
    (crossover/combine (crossover/->SimpleCrossover (fn [_ _] (is false "operator invoked but not expected")))
                       [:chromosome] ))
  (testing "returns an empty vector on a population of one element"
    (is (= []
           (crossover/combine (create-throwing-crossover) [])))))

(defn- crossover-on-two-elements [counter p-cross expected-random-value]
  (let [conversion {:chromosome1 :result1
                    :chromosome2 :result2}
        simple-crossover (crossover/->SimpleCrossover (fn [a b]
                                                        (swap! counter inc)
                                                        (is (and (= :chromosome1 a)
                                                                 (= :chromosome2 b)))
                                                        (vector (get conversion a)
                                                                (get conversion b))))]
    (crossover/combine simple-crossover [:chromosome1 :chromosome2])))

(deftest one-pair
  (testing "crossover applies the operator once on the chromosomes"
    (let [counter (atom 0)]
      (crossover-on-two-elements counter 1 0.5)
      (is (= 1 @counter) "Crossover operator not executed the expected number of times")))
  (testing "for two chromosomes returns the output of the crossover function"
    (let [counter (atom 0)]
      (is (= [:result1 :result2]
             (crossover-on-two-elements counter 1 1))))))

(deftest multiple-values
  (testing "general case: multiple chromosomes"
    (let [crossover (crossover/->SimpleCrossover (fn [a b] (vector (+ a 10) (- b 10))))]
      (is (= [11 -8 13 -6 15 -4 17 -2]
             (crossover/combine crossover [1 2 3 4 5 6 7 8]))))))

(defn test-crossover-output-for-p-and-random-values [probability random-value]
  (let [crossing (crossover/create-classic-crossover #(vector (str %) (str %2)) 1 #(identity 1))]
    (is (= [":a" ":b"] (crossover/combine crossing [:a :b])))))

(defn- dumb-crossover-operator [a b]
  (vector (str a) (str b)))

(defn- create-dumb-crossover [probability random-f]
  (crossover/create-classic-crossover dumb-crossover-operator probability random-f))

(deftest probability-based-crossover
  (testing "applies the function on every pair for P=1, no matter the outcome of the random-f"
    (let [crossing (create-dumb-crossover 1 #(identity 0))]
      (is (= [":a" ":b"] (crossover/combine crossing [:a :b]))))
    (let [crossing (create-dumb-crossover 1 #(identity 0.5))]
      (is (= [":a" ":b"] (crossover/combine crossing [:a :b]))))
    (let [crossing (create-dumb-crossover 1 #(identity 0.9999))]
      (is (= [":a" ":b"] (crossover/combine crossing [:a :b])))))
    (testing "never applies the function for P=0, no matter the outcome of the random-f"
    (let [crossing (create-dumb-crossover 0 #(identity 0))]
      (is (= [:a :b] (crossover/combine crossing [:a :b]))))
    (let [crossing (create-dumb-crossover 0 #(identity 0.5))]
      (is (= [:a :b] (crossover/combine crossing [:a :b]))))
    (let [crossing (create-dumb-crossover 0 #(identity 1))]
      (is (= [:a :b] (crossover/combine crossing [:a :b])))))
  (testing "general case"
    (let [random-f (utils/create-iterator [0.1 0.8 0.3 0.2 0.9])
          crossing (create-dumb-crossover 0.3 random-f)]
      (is (= [":a" ":b" :c :d :e :f ":g" ":h" :i :l]
             (crossover/combine crossing [:a :b :c :d :e :f :g :h :i :l]))))))

(deftest single-point-vector-crossover-test
  (testing "returns the two instances unchanged if both empty vectors, probability = 1"
    (let [vector-crossover (crossover/create-1p-vector-crossover 1.0
                                                                 rand-int
                                                                 #(identity 0))]
      (is (= [[] []]
             (crossover/combine vector-crossover [[] []])))))
  (testing "with one empty chromosome and one that isn't, split the genome, probability = 1"
    (let [vector-crossover (crossover/create-1p-vector-crossover 1.0
                                                                 (utils/create-iterator [0 2])
                                                                 #(identity 0))]
      (is (= [[:a :b]
              [:c :d :e]]
             (crossover/combine vector-crossover [[]
                                                  [:a :b :c :d :e]])))))
  (testing "general case, probability = 1"
    (let [vector-crossover (crossover/create-1p-vector-crossover 1.0
                                                                 (utils/create-iterator [3 4])
                                                                 #(identity 0))]
      (is (= [[:a :b :c :d 4 5 6 7]
              [1 2 3 :e]]
             (crossover/combine vector-crossover [[1 2 3 4 5 6 7]
                                                  [:a :b :c :d :e]])))))
  (testing "general case, probability 0.5, various random numbe extractions"
    (let [vector-crossover (crossover/create-1p-vector-crossover 0.5
                                                                 (utils/create-iterator [3 4])
                                                                 #(identity 0.2))]
      (is (= [[:a :b :c :d 4 5 6 7]
              [1 2 3 :e]]
             (crossover/combine vector-crossover [[1 2 3 4 5 6 7]
                                                  [:a :b :c :d :e]]))))
    (let [vector-crossover (crossover/create-1p-vector-crossover 0.5
                                                                 (utils/create-iterator [3 4])
                                                                 #(identity 0.5))]
      (is (= [[1 2 3 4 5 6 7]
              [:a :b :c :d :e]]
             (crossover/combine vector-crossover [[1 2 3 4 5 6 7]
                                                  [:a :b :c :d :e]]))))
    (let [vector-crossover (crossover/create-1p-vector-crossover 0.5
                                                                 (utils/create-iterator [3 4])
                                                                 #(identity 0.8))]
      (is (= [[1 2 3 4 5 6 7]
              [:a :b :c :d :e]]
             (crossover/combine vector-crossover [[1 2 3 4 5 6 7]
                                                  [:a :b :c :d :e]]))))))

(defmacro test-certain-tree-crossover [input expected-output]
  `(let [tree-crossover# (crossover/create-tree-crossover 1.0 #(identity 0.0))]
    (is (= ~expected-output
           (crossover/combine tree-crossover# ~input)))))

(deftest count-split-points
  (testing "split points of a non list returns 1"
    (is (= 1 (crossover/count-split-points 0)))
    (is (= 1 (crossover/count-split-points :x))))
  (testing "split points of a list with an unary operator is 2"
    (is (= 2 (crossover/count-split-points '(Math/sqrt 2)))))
  (testing "split points of a two arguments operation is 3"
    (is (= 3 (crossover/count-split-points '(+ :a :b)))))
  (testing "split points of a nested list is the recursive sum of each split point"
    (is (= 8 (crossover/count-split-points '(+ (- 3 4) (* 1 (Math/sqrt 12))))))))


(deftest all-split-points-test
  (testing "iterating over a value "
    (is (= [[:x 42]]
           (crossover/all-split-points 42 :x))))
  (testing "iterating over a list"
    (is (= [[:x '(+ 1 2)] ['(+ :x 2) 1] ['(+ 1 :x) 2]]
           (crossover/all-split-points '(+ 1 2) :x))))
  (testing "iterate: general case"
    (is (= [[:x '(+ 1 (- 3 (* 1 5)) (Math/sqrt 12))]
            ['(+ :x (- 3 (* 1 5)) (Math/sqrt 12)) 1]
            ['(+ 1 :x (Math/sqrt 12)) '(- 3 (* 1 5))]
            ['(+ 1 (- :x (* 1 5)) (Math/sqrt 12)) 3]
            ['(+ 1 (- 3 :x) (Math/sqrt 12)) '(* 1 5)]
            ['(+ 1 (- 3 (* :x 5)) (Math/sqrt 12)) 1]
            ['(+ 1 (- 3 (* 1 :x)) (Math/sqrt 12)) 5]
            
            ['(+ 1 (- 3 (* 1 5)) :x) '(Math/sqrt 12)]            
            ['(+ 1 (- 3 (* 1 5)) (Math/sqrt :x)) 12]]
           (crossover/all-split-points '(+ 1 (- 3 (* 1 5)) (Math/sqrt 12)) :x)))))


(comment ;;; are those two tests at odds for the order of the results?
  (deftest split-at-point-test
   (testing "splitting a form at place 0 returns 0 and the original form"
     (is (= [:x '(+ 1 2)] (crossover/split-at-point '(+ 1 2) 0 :x)))
     (is (= [:x 42] (crossover/split-at-point 42 0 :x))))
   (testing "spitting a form at an argument returns the argument and a form with replaced symbol"
     (is (= [(list + :x 2) 1]
            (crossover/split-at-point '(+ 1 2) 1 :x))))))


(comment
  (deftest tree-crossover-test
   (testing "when combining empty chromosomes, the result is an empty chromosome"
     (test-certain-tree-crossover ['() '()] ['() '()]))
   (testing "when combining lists ..."
     ;;TBD
     )))
