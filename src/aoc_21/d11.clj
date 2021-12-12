(ns aoc-21.d11
  (:require [aoc-21.base :as b]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [medley.core :as m]
            [clojure.set :as set]))

;; You enter a large cavern full of rare bioluminescent dumbo octopuses! They seem to not like the Christmas lights on your submarine, so you turn them off for now.

;; There are 100 octopuses arranged neatly in a 10 by 10 grid. Each octopus slowly gains energy over time and flashes brightly for a moment when its energy is full. Although your lights are off, maybe you could navigate through the cave without disturbing the octopuses if you could predict when the flashes of light will happen.

;; Each octopus has an energy level - your submarine can remotely measure the energy level of each octopus (your puzzle input). For example:

;; 5483143223
;; 2745854711
;; 5264556173
;; 6141336146
;; 6357385478
;; 4167524645
;; 2176841721
;; 6882881134
;; 4846848554
;; 5283751526

;; The energy level of each octopus is a value between 0 and 9. Here, the top-left octopus has an energy level of 5, the bottom-right one has an energy level of 6, and so on.

(s/def ::octopus nat-int?)
(s/def ::energy-level nat-int?)

;; You can model the energy levels and flashes of light in steps. During a single step, the following occurs:

(s/def ::coord (s/tuple int? int?))
(s/def ::coords (s/coll-of ::coord
                           :kind set?))

(s/def ::flashing ::coords)
(s/def ::flashed ::coords)

;; sorted by y then x, both ascendi
(def initial-octopi (sorted-map-by (fn [a b]
                                     (let [[ax ay] a
                                           [bx by] b]
                                       (or (< ay by)
                                           (and (= ay by)
                                                (< ax bx)))))))
(s/def ::octopi (s/every-kv ::coord ::octopus
                            :into initial-octopi))

(s/fdef parse-input
  :args (s/cat :input string?)
  :ret ::octopi)
(defn parse-input
  [octopi-str]
  (b/parse-grid-str->map b/char->int initial-octopi octopi-str))

(s/fdef octopi->output
  :args (s/cat :octopi ::octopi)
  :ret string?)
(defn octopi->output
  "octopi must be built upon initial-octopi (or using the same sort fn) or order might be broken"
  [octopi]
  (println octopi)
  (let [xf (comp (map #(update % 0 second)) ;; coord->y
                 (partition-by first) ;; group by y
                 (map #(eduction (map second) %))
                 (map str/join)
                 (interpose "\n"))]
    (transduce xf str (seq octopi))))

(s/def ::octopi-state
  (s/keys :req-un [::octopi] :opt-un [::flashed ::flashing]))

;; 1. First, the energy level of each octopus increases by 1.

(s/fdef octopi-energy-increase
        ;; increases energy and checks for flashing
  :args (s/cat :octopi-state ::octopi-state)
  :ret ::octopi-state) ;; returns octopi and flashing coords

;; 2. Then, any octopus with an energy level greater than 9 flashes. This increases the energy level of all adjacent octopuses by 1, including octopuses that are diagonally adjacent. If this causes an octopus to have an energy level greater than 9, it also flashes. This process continues as long as new octopuses keep having their energy level increased beyond 9. (An octopus can only flash at most once per step.)

(s/fdef octopi-flash
        ;; takes octopi, flashing coords and already flashed coords, must recursively flash the affected octopi
  :args (s/cat :octopi-flashing ::octopi-state)
  :ret ::octopi-state) ;; returns octopi and flashed coords

;; (s/fdef recursive-flash-octopi-map)

(defn flashing?
  [energy-level]
  (> energy-level 9))

;; 3. Finally, any octopus that flashed during this step has its energy level set to 0, as it used all of its energy to flash.

(s/fdef octopi-lights-out
        ;; takes octopi and flashed
  :args (s/cat :octopi-flashed ::octopi-state)
  :ret ::octopi-state) ;; returns octopi



;; Adjacent flashes can cause an octopus to flash on a step even if it begins that step with very little energy. Consider the middle octopus with 1 energy in this situation:

;; Before any steps:
;; 11111
;; 19991
;; 19191
;; 19991
;; 11111

;; After step 1:
;; 34543
;; 40004
;; 50005
;; 40004
;; 34543

;; After step 2:
;; 45654
;; 51115
;; 61116
;; 51115
;; 45654

;; An octopus is highlighted when it flashed during the given step.

;; Here is how the larger example above progresses:

;; Before any steps:
(def simple-input
  "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

;; After step 1:
;; 6594254334
;; 3856965822
;; 6375667284
;; 7252447257
;; 7468496589
;; 5278635756
;; 3287952832
;; 7993992245
;; 5957959665
;; 6394862637

;; After step 2:
;; 8807476555
;; 5089087054
;; 8597889608
;; 8485769600
;; 8700908800
;; 6600088989
;; 6800005943
;; 0000007456
;; 9000000876
;; 8700006848

;; After step 10:
;; 0481112976
;; 0031112009
;; 0041112504
;; 0081111406
;; 0099111306
;; 0093511233
;; 0442361130
;; 5532252350
;; 0532250600
;; 0032240000

;; After step 10, there have been a total of 204 flashes. Fast forwarding, here is the same configuration every 10 steps:

;; After step 20:
;; 3936556452
;; 5686556806
;; 4496555690
;; 4448655580
;; 4456865570
;; 5680086577
;; 7000009896
;; 0000000344
;; 6000000364
;; 4600009543

;; After step 100:
;; 0397666866
;; 0749766918
;; 0053976933
;; 0004297822
;; 0004229892
;; 0053222877
;; 0532222966
;; 9322228966
;; 7922286866
;; 6789998766

;; After 100 steps, there have been a total of 1656 flashes.

;; Given the starting energy levels of the dumbo octopuses in your cavern, simulate 100 steps. How many total flashes are there after 100 steps?

(def initial-octopi-state
  {:octopi initial-octopi
   :flashing #{}
   :flashed #{}})

(defn rf-energy-increase
  ([state] state)
  ([state [coord energy-level]]
   (let [res (assoc-in state [:octopi coord] energy-level)]
     (if (flashing? energy-level)
       (update res :flashing conj coord)
       res))))

(defn octopi-energy-increase
  ([{:keys [octopi]}]
   (transduce (map #(update % 1 inc)) rf-energy-increase (assoc initial-octopi-state :octopi octopi) octopi)))

(s/fdef selected-octopi-energy-increase
  :args (s/cat :octopi-state ::octopi-state :coords ::coords))

(defn selected-octopi-energy-increase
  [octopi-state coords]
  (let [octopi (:octopi octopi-state)
        octopi-entries (sequence (comp (map (juxt identity octopi))
                                       (filter (fn [[_ v]] v)))
                                 coords)]
    (transduce (map #(update % 1 inc))
               (completing rf-energy-increase
                           #(update % :octopi
                                    (partial merge octopi)))
               octopi-state
               octopi-entries)))

(defn- get-adjacent-dimension-range
  [x]
  (range (dec x) (+ x 2)))

(defn get-adjacent-coords
  "also returns the given coord"
  [[x y]]
  (for [x (get-adjacent-dimension-range x)
        y (get-adjacent-dimension-range y)]
    [x y]))

(defn- rf-flash-octopi-map
  [{:keys [flashed flashing] :as octopi-state}
   flashing-coord]
  (let [adjacent (into #{} (get-adjacent-coords flashing-coord))
        flash-impact-coords (set/difference adjacent flashing flashed #{flashing-coord})
        res (selected-octopi-energy-increase octopi-state flash-impact-coords)
        {n-octopi :octopi, n-flashing :flashing} res]
    {:flashed (into flashed (conj n-flashing flashing-coord))
     :flashing n-flashing
     :octopi n-octopi}))

(defn octopi-flash
  ([{:keys [flashing],, :as octopi-state}]
   (let [initial-state (update (assoc octopi-state :flashing #{})
                               :flashed into flashing)
         res (reduce rf-flash-octopi-map initial-state flashing)]
     (if (empty? (:flashing res))
       res
       (recur res)))))

(defn octopi-lights-out
  ([{:keys [octopi flashed] :as state}]
   (assoc state
          :octopi (reduce #(assoc %1 %2 0) octopi flashed)
          ;; :flashed #{}
          :flashing #{})))

(def step (comp octopi-lights-out octopi-flash octopi-energy-increase))

(def res1-day 100)

(defn main
  [input]
  (let [octopi (parse-input input)
        octopi-state (assoc initial-octopi-state :octopi octopi)
        states (iterate step octopi-state)
        octopi-count (count octopi)
        xf1 (comp (take (inc res1-day))
                  (map :flashed)
                  (map count))
        res1 (transduce xf1 + states)
        xf2 (comp (map :flashed)
                  (map count)
                  (m/indexed)
                  (filter (fn [[_ flashes]]
                            (= flashes octopi-count))))
        res2 (transduce xf2 (b/rf-first #(first %)) nil states)]
    [res1 res2]))

(defn -main
  []
  (main (b/get-input 11)))

(stest/instrument)



;; something wrong with specs and the recursive flashing