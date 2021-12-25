(ns aoc-21.d6
  (:require [aoc-21.base :as b]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [medley.core :as m]))

(def after-hatching-reset-age 6)

(def hatchling-age 8)

(defn day
  [fish-count-map]
  (let [aged-fish (m/map-keys dec fish-count-map)]
    (if-let [hatching-count (get aged-fish -1)]
      (let [add-hatching #((fnil +' 0) % hatching-count)]
        (-> aged-fish
            (b/updates after-hatching-reset-age add-hatching
                       hatchling-age add-hatching)
            (dissoc -1)))
      aged-fish)))

(defn parse-input
  [input]
  (transduce (mapcat #(str/split % #","))
             (b/get-rf-count-map #(m/map-keys b/parse-int %))
             [input]))

(def simple-input "3,4,3,1,2")

(defn count-fish
  [fish-map]
  (reduce +' (vals fish-map)))

(defn main
  [input day-n]
  (let [initial-state (parse-input input)
        days (iterate day initial-state)
        resulting-fish-map (nth days day-n)
        fish-count (count-fish resulting-fish-map)]
    fish-count))

(def day-to-simulate-1 80)

(def day-to-simulate-2 256)

(defn -main
  []
  (let [input (b/get-input 6)]
    (mapv #(main input %) [day-to-simulate-1 day-to-simulate-2])))
