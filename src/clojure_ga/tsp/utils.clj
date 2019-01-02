(ns clojure-ga.tsp.utils)

(defn- subsets-of-size [xo N]
  (map (fn [subset]
         (apply hash-set subset))
       (partition  N 1 xo)))

(defn- subsets-sizes [xo]
  (reverse (range 2 (count xo))))

;;; TODO/FIXME: can that be done lazily, as an exercise?
(defn- subsets [xo]
  (apply concat (map #(subsets-of-size xo %)
                     (subsets-sizes xo))))

(defn- split-subset [xo subset]
  (let [indices (map #(.indexOf xo %) subset)
        start (apply min indices)
        end (apply max indices)]
    (vector (subvec xo 0 start)
            (subvec xo start (inc end))
            (subvec xo (inc end)))))

(defn common-subsets [xa xb]
  (let [sa (apply hash-set (subsets xa))
        sb (subsets xb)]
    (filter #(contains? sa %) sb)))

(defn replace-subset [a b subset]
  (let [[a-head a-middle a-tail] (split-subset a subset)
        [b-head b-middle b-tail] (split-subset b subset)]
    (vector (vec (concat a-head b-middle a-tail))
            (vec (concat b-head a-middle b-tail)))))

(defn cross-sequences [xa xb rand-nth]
  (let [subsets (common-subsets xa xb)]
    (if (empty? subsets)
      (vector xa xb)
      (replace-subset xa xb (rand-nth subsets)))))
