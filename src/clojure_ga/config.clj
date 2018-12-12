(ns clojure-ga.config
  (:require [clojure-ga.classic :as classic]
            [clojure-ga.algorithm :as algorithm]
            [clojure-ga.simulator :as simulator]))

(def default-p-cross 0.2)
(def default-m-cross 0.001)

;;; TODO/FIXME rename to Config
(defrecord Config [p-cross
                   p-mutation
                   op-cross
                   op-mutation
                   op-generation
                   random-generator])

(defprotocol ConfigValidator
  (validate [this]))

(defn- p-out-of-range? [p]
  (or (< p 0) (> p 1)))

(defn- validate-arguments [config]
  (cond
    (nil? (:random-generator config)) (throw (IllegalArgumentException. "missing random generator"))
    (nil? (:p-cross config)) (throw (IllegalArgumentException. "missing cross probability"))
    (p-out-of-range? (:p-cross config)) (throw (IllegalArgumentException. "cross probability out of range"))
    (nil? (:p-mutation config)) (throw (IllegalArgumentException. "missing mutation probability"))
    (p-out-of-range? (:p-mutation config)) (throw (IllegalArgumentException. "mutation probability out of range"))
    (nil? (:op-mutation config)) (throw (IllegalArgumentException. "missing mutation function"))
    (nil? (:op-cross config)) (throw (IllegalArgumentException. "missing crossing function"))
    (nil? (:op-generation config)) (throw (IllegalArgumentException. "missing generator function"))
    (nil? (:op-fitness config)) (throw (IllegalArgumentException. "missing fitness function"))
    :default config))

(extend-type Config
  ConfigValidator
  (validate [this] (validate-arguments this)))

(defn create-config [ & parameters]
  (validate
   (map->Config (merge {:random-generator rand
                        :p-cross default-p-cross
                        :p-mutation default-m-cross}
                       (apply hash-map parameters)))))

(defn add-first-generation-solutions [config sequence]
  (assoc config :first-generation sequence))

(defrecord Simulation [config algorithm population])

(defn create-simulation
  "Create a new Simulation instance"
  ([config algorithm]
   (create-simulation config algorithm []))
  ([config algorithm population]
   (->Simulation config algorithm population)))

(defprotocol PopulationProvider
  (addInstance [this] "add a new instance to the population"))

(defn- create-instance [simulation]
  ((:op-generation (:config simulation))))

(defn- add-instance [simulation]
  (update simulation
          :population  #(conj % (create-instance simulation))))

(extend-type Simulation
  PopulationProvider
  (addInstance [this]
    (add-instance this))
  simulator/Simulator
  (step [this]
    (map->Simulation (algorithm/advance (:algorithm this) this))))


