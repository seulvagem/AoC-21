(ns aoc-21.d9
  (:require [aoc-21.base :as b]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [medley.core :as m]))



(def simple-input "2199943210
3987894921
9856789892
8767896789
9899965678")

(defn parse-input
  [input-lines]
  (mapv (partial mapv #(Character/digit % 10)) input-lines))

(defn get-coordinates
  [grid]
  (let [height (count grid)
        length (count (first grid))]
    (for [y (range height)
          x (range length)]
      [y x])))

(defn valid-basin
  [x val]
  (and x (not= x 9) (> x val)))

(defn single-basin
  [get-grid coords-to-ignore coord]
  (let [val (get-grid coord)
        xf (comp (filter #(not (coords-to-ignore %)))
                 (filter #(valid-basin (get-grid %) val)))]
    (sequence xf (b/get-adjacent-coords coord))))

(defn basin
  ([get-grid coord]
   (basin #{coord} get-grid [coord]))
  ([acc-coords get-grid coords]
   (let [coords-to-ignore (into acc-coords coords)
         xf (mapcat #(single-basin get-grid coords-to-ignore %))
         new-coords (into #{} xf coords)]
     (if (empty? new-coords)
       acc-coords
       (recur (into acc-coords new-coords) get-grid new-coords)))))

(defn low-point?
  [get-grid coord]
  (let [height (get-grid coord)
        xf (comp (map get-grid)
                 (filter identity))
        adj-heights (sequence xf (b/get-adjacent-coords coord))]
    (every? #(< height %) adj-heights)))
;;   (map (juxt identity b/get-adjacent-coords))
;;   (map #(b/updates-list % [get-grid (partial map get-grid)]))
;;   (map #(b/updates-list % [identity (partial filter identity)]))
;;   (filter (fn [[height adj-heights]]
;;             (every? #(< height %) adj-heights)))

;; (def sorted-set-desc
;;   (sorted-set-by ))

(defn rf-max3-product
  ([] [])
  ([max3]
   (println max3)
   (reduce * max3))
  ([max3 n]
   (take 3 (sort-by identity #(> %1 %2) (conj max3 n)))))

(defn main
  [input-lines]
  (let [grid (parse-input input-lines)
        coords (get-coordinates grid)
        get-grid (partial get-in grid)
        xf-filter-in-low-points (filter (partial low-point? get-grid))
        xf-res1 (comp xf-filter-in-low-points
                      (map get-grid)
                      (map inc))
        xf-res2 (comp xf-filter-in-low-points
                      (map (partial basin get-grid))
                      (map count))
        res1 (transduce xf-res1 + coords)
        res2 (transduce xf-res2 rf-max3-product coords)]
    ;; (into [] xf coords)
    [res1 res2]))

(defn -main
  []
  (main (b/get-split-input 9)))