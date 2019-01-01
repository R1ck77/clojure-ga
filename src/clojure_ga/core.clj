(ns clojure-ga.core
  (:require 
            [clojure-ga.words.words :as words])
  (:gen-class))

(defn -main
  [& args]
  (let [results (words/simulation (Integer/valueOf (or (first args) 10))
                                  (Integer/valueOf (or (second args) 100))
                                  (Integer/valueOf (or (get args 2) 1000)))]
    (doall
     (map (fn [[score result]]
            (println (str score " : " (apply str result))))
          (reverse (take 10 (sort-by #(- (first %)) results))))))
  (shutdown-agents))
