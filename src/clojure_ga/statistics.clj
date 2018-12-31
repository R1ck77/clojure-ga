(ns clojure-ga.statistics)

(defn- std-dev [sum values]
  (let [N (count values)
        squares (reduce #(+ % (* %2 %2)) 0 values)]
    (Math/sqrt (/ (- (* N squares) (* sum sum))
                  (* N (- N 1))))))

(defn mean-std-dev [values]
  (let [N (count values)
        sum (reduce + values)]
    (if (< N 2)    
      (throw (ArithmeticException. ))
      (vector (/ sum N)
              (std-dev sum values)))))

 
