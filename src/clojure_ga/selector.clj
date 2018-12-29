(ns clojure-ga.selector)

(defprotocol Selector
  (select [this population]))
