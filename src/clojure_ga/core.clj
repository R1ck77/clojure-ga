(ns clojure-ga.core
  (:require [clojure.string :as string]
            [clojure-ga.evolutionary.evolutionary :as evo]))

(defn- read-line [line-s]
  (vec (map #(Double/valueOf %) (string/split line-s #"\s"))))

(defn- read-points [file]
  (vec (map read-line (string/split-lines (slurp file)))))

(defn -main
  [& args]
  (dorun
   (map (fn [[score formula]]
          (println score " -> " formula))
        (sort-by #(first %)
                 (evo/simulation (read-points (first args))
                                 10 1000 1e6))))
  (shutdown-agents))
