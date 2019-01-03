(ns clojure-ga.core
  (:require [clojure-ga.tsp.demo :as demo])
  (:gen-class))

(defn -main
  [& [N-string generations-s size-s cross-range-s mutation-range-s repetitions-s]]
  (dorun
   (map (fn [[[x y] result]]
          (println x y result))
        (take 10 (demo/best-meta-p (Integer/valueOf N-string)
                                   (Integer/valueOf generations-s)
                                   (Integer/valueOf size-s)
                                   (eval (read-string cross-range-s))
                                   (eval (read-string mutation-range-s))
                                   (Integer/valueOf repetitions-s)))))
  (shutdown-agents))
