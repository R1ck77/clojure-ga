(ns clojure-ga.mutation)

(defn mutation [population mutation-f]
  (vec
   (map mutation-f population)))
