
(ns oi
  (:require [clojure.set :as set]))
(defn powerset [items] ;; by reborg
  (reduce
    (fn [s x]
      (set/union s (map #(conj % x) s)))
    (hash-set #{})
    items))