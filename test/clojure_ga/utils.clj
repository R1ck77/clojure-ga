(ns clojure-ga.utils)

(defn create-iterator
  "Returns a function that iterates over the elements and then returns identically nil"
  [elements]
  (let [remaining (atom elements)]
    (fn []
      (let [next (first @remaining)]
        (swap! remaining rest)
        next))))
