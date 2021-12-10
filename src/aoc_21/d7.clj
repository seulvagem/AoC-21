(ns aoc-21.d7
  (:require [aoc-21.base :as b]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [medley.core :as m]))

(defn parse-input
  [input]
  (let [positions (str/split input #",")]
    (transduce (map b/parse-int) (b/get-rf-count-map) positions)))

(def simple-input "16,1,2,0,4,2,7,1,2,14")

(defn fuel-needed
  [f m ref-p]
  (reduce (fn [total-fuel [p c]]
            (-> p (- ref-p) b/abs f (* c) (+ total-fuel))) 0 m))

(defn fuel-list
  [f m]
  (let [[max-p] (apply (partial max-key #(get % 0)) m)]
    (map (partial fuel-needed f m) (range (inc max-p)))))

(defn get-least-fuel
  [f m]
  (let [fuel-list (fuel-list f m)]
    (reduce (fn [a b]
              (if (> b a)
                (reduced a)
                b)) fuel-list)))

(def fuel-progression
  (map second (iterate (fn [[i f]]
                         [(inc i) (+ i f)]) [1 0])))

(defn get-fuel2-for-dist
  [dist]
  (nth fuel-progression dist))

(defn main
  [input]
  (let [pos-count-map (parse-input input)
        res1 (get-least-fuel identity pos-count-map)
        res2 (get-least-fuel get-fuel2-for-dist pos-count-map)]
    [res1 res2]))

(defn -main
  []
  (main (b/get-input 7)))