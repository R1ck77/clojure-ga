(ns clojure-ga.engine)

(defn- p-out-of-range? [p]
  (or (< p 0) (> p 1)))

(defn- validate [engine]
  (cond
    (nil? (:p-cross engine)) (throw (IllegalArgumentException. "missing cross probability"))
    (p-out-of-range? (:p-cross engine)) (throw (IllegalArgumentException. "cross probability out of range"))
    (nil? (:p-mutation engine)) (throw (IllegalArgumentException. "missing mutation probability"))
    (p-out-of-range? (:p-mutation engine)) (throw (IllegalArgumentException. "mutation probability out of range"))
    (nil? (:op-mutation engine)) (throw (IllegalArgumentException. "missing mutation function"))
    (nil? (:op-cross engine)) (throw (IllegalArgumentException. "missing crossing function"))
    :default engine))

(defn create [ & parameters]
  (validate (apply hash-map parameters)))
