(ns clojure-ga.engine-test
  (:require [clojure.test :refer :all]
            [clojure-ga.engine :as engine]))

(def reasonable-default-arguments [:p-cross 0.75
                                   :p-mutation 0.01
                                   :op-cross (fn [a b] a)
                                   :op-mutation (fn [a] a)])

(def missing-p-cross [:p-mutation 0.01 :op-cross (fn [a b] a) :op-mutation (fn [a] a)])
(def missing-p-mutation [:p-cross 0.75 :op-cross (fn [a b] a) :op-mutation (fn [a] a)])
(def missing-op-cross [:p-cross 0.75 :p-mutation 0.01 :op-mutation (fn [a] a)])
(def missing-op-mutation [:p-cross 0.75 :p-mutation 0.01 :op-cross (fn [a b] a)])


(deftest engine-creation
  (testing "the engine can be instantiated"
    (is (apply engine/create reasonable-default-arguments)))
  (testing "missing parameters are met win an exception"
    (is (thrown? IllegalArgumentException (apply engine/create missing-p-cross)))
    (is (thrown? IllegalArgumentException (apply engine/create missing-p-mutation)))
    (is (thrown? IllegalArgumentException (apply engine/create missing-op-cross)))
    (is (thrown? IllegalArgumentException (apply engine/create missing-op-mutation)))))
