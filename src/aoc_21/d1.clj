(ns aoc-21.d1
  (:require [aoc-21.base :as b]))

(defn rf-count
  ([] 0)
  ([acc] acc)
  ([acc _] (inc acc)))

(defn -main
  []
  (let [str-depths (b/get-split-input 1)
        xf-parse (map b/parse-int)
        xf-filter-increasing-pairs (comp (b/xf-partition 2)
                                         (filter #(apply < %)))
        xf-res1 (comp xf-parse
                      xf-filter-increasing-pairs)
        xf-res2 (comp xf-parse
                      (b/xf-partition 3)
                      (map #(apply + %))
                      xf-filter-increasing-pairs)
        res1 (transduce xf-res1 rf-count str-depths)
        res2 (transduce xf-res2 rf-count str-depths)]
    [res1 res2]))
