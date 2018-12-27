(ns clojure-ga.core
  (:require [clojure.string :as string]
            [clojure-ga.evolutionary.evolutionary :as evo])
  (:gen-class))

(defn- read-line [line-s]
  (vec (map #(Double/valueOf %) (string/split line-s #"\s"))))

(defn- read-points [file]
  (vec (map read-line (string/split-lines (slurp file)))))

(defn -main
  [& args]
  (let [file (first args)
        steps (Integer/valueOf (or (second args) "10"))
        size (Integer/valueOf (or (nth args 2) "1000"))]
    (dorun
           (map (fn [[score formula]]
                  (println score " -> " formula))
                (sort-by #(first %)
                         (evo/simulation (read-points (first args))
                                         steps size 1e6)))))
  (shutdown-agents))
