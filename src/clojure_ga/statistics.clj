(ns clojure-ga.statistics)

(defn mean-std-dev [values]
  (let [N (count values)]
    (if (< N 2)    
      (throw (ArithmeticException. ))
      (vector (/ (reduce + values) N) 0))))

 
