(ns clojure-ga.engine)

(defn- validate [engine]
  (cond
    (nil? (:p-cross engine)) (throw (IllegalArgumentException. "missing cross probability"))
    (nil? (:p-mutation engine)) (throw (IllegalArgumentException. "missing mutation probability"))
    (nil? (:op-mutation engine)) (throw (IllegalArgumentException. "missing mutation function"))
    (nil? (:op-cross engine)) (throw (IllegalArgumentException. "missing crossing function"))
    :default engine))

(defn create [ & parameters]
  (validate (apply hash-map parameters)))
