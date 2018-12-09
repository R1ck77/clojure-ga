(ns clojure-ga.engine)

(def default-p-cross 0.2)
(def default-m-cross 0.001)

;;; TODO/FIXME rename to Config
(defrecord Engine [p-cross
                   p-mutation
                   op-cross
                   op-mutation
                   op-generation
                   random-generator])

(defprotocol ConfigValidator
  (validate [this]))

(defn- p-out-of-range? [p]
  (or (< p 0) (> p 1)))

(defn- validate-arguments [engine]
  (cond
    (nil? (:random-generator engine)) (throw (IllegalArgumentException. "missing random generator"))
    (nil? (:p-cross engine)) (throw (IllegalArgumentException. "missing cross probability"))
    (p-out-of-range? (:p-cross engine)) (throw (IllegalArgumentException. "cross probability out of range"))
    (nil? (:p-mutation engine)) (throw (IllegalArgumentException. "missing mutation probability"))
    (p-out-of-range? (:p-mutation engine)) (throw (IllegalArgumentException. "mutation probability out of range"))
    (nil? (:op-mutation engine)) (throw (IllegalArgumentException. "missing mutation function"))
    (nil? (:op-cross engine)) (throw (IllegalArgumentException. "missing crossing function"))
    (nil? (:op-generation engine)) (throw (IllegalArgumentException. "missing generator function"))
    (nil? (:op-fitness engine)) (throw (IllegalArgumentException. "missing fitness function"))
    :default engine))

(extend-type Engine
  ConfigValidator
  (validate [this] (validate-arguments this)))

(defn create-engine [ & parameters]
  (validate
   (map->Engine (merge {:random-generator rand
                        :p-cross default-p-cross
                        :p-mutation default-m-cross}
                       (apply hash-map parameters)))))

(defn add-first-generation-solutions [engine sequence]
  (assoc engine :first-generation sequence))

(defrecord Simulation [engine algorithm population])

(defn create-simulation
  "Create a new Simulation instance"
  ([engine algorithm]
   (create-simulation engine algorithm []))
  ([engine algorithm population]
   (->Simulation engine algorithm population)))

(defprotocol PopulationProvider
  (addInstance [this] "add a new instance to the population"))

(defn- create-instance [simulation]
  ((:op-generation (:engine simulation))))

(defn- add-instance [simulation]
  (update simulation
          :population  #(conj % (create-instance simulation))))

(defprotocol Simulator
  (step [this] "execute a simulation step. Returns a new simulation"))

(defprotocol Algorithm
;;; TODO/FIXME use the associative abstraction!
  (advance [algorithm config population]
    "Peform a step of a given simulation, returns a map with population, algorithm and configuration"))

(extend-type Simulation
  PopulationProvider
  (addInstance [this]
    (add-instance this))
  Simulator
  (step [this]
    (let [{:keys [engine algorithm population]} (advance (:algorithm this) (:engine this) (:population this))]
      (create-simulation engine algorithm population))))


