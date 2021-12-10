(ns aoc-21.d3
  (:require [aoc-21.base :as b]
            [medley.core :as m]
            [clojure.string :as str]))

(defn reverse-binary-number
  [bin]
  (mapv #(if (zero? %)
           1
           0) bin))

(defn matrix-sum
  [mat mat2]
  (let [xf (comp (m/indexed)
                 (map (fn [[i n]]
                        (+ n (get mat2 i 0)))))]
    (into [] xf mat)))

(defn rf-get-gamma-&-epsilon
  ([] {:counts-of-1 [], :n-count 0})
  ([{:keys [counts-of-1 n-count]}]
   (let [half (/ n-count 2)
         gamma (mapv #(if (>= % half)
                        1
                        0) counts-of-1)
         epsilon (reverse-binary-number gamma)]
     [gamma epsilon]))
  ([count-state bin]
  ;;  (println bin count-state)
   (b/updates count-state
              :counts-of-1 #(matrix-sum bin %)
              :n-count inc)))

(defn most-common-at
  [bins i]
  (let [half (/ (count bins) 2)
        ones (transduce (map #(get % i)) + 0 bins)]
    (if (>= ones half)
      1
      0)))

(defn bit-indicess-by
  [bins i]
  (transduce (comp (map #(get % i))
                   (m/indexed))
             (completing (fn [indices [i b]]
                           (let [upd-k (if (= b \0)
                                         :0
                                         :1)]
                             (update indices upd-k conj i))))
             {:0 []
              :1 []} bins))

(defn filter-bins-by
  [f bins i]
  (let [indices (as-> bins $
                  (bit-indicess-by $ i)
                  ;; (map #(% $) [:0 :1])
                  (let [zero-indices (:0 $)
                        one-indices (:1 $)]
                    (if (f (count zero-indices) (count one-indices))
                      zero-indices
                      one-indices)))]
    (mapv #(get bins %) indices)))

(defn get-res2-by
  [binary-numbers f]
  (let [length (count (first binary-numbers))
        [res] (reduce (fn [bins i]
                        (let [res (filter-bins-by f bins i)]
                          (if (= (count res) 1)
                            (reduced res)
                            res)))
                      binary-numbers (range length))]
    res))

(defn get-oxygen-&-co2
  [bins]
  (mapv (partial get-res2-by bins) [> <=]))

(defn calc-res
  [bs]
  (transduce (map #(b/parse-int % 2)) * bs))

(defn -main
  []
  (let [binary-numbers (b/get-split-input 3)
        xf-char->int (comp (map #(mapv b/char->int %)))
        gamma&epsilon (transduce xf-char->int
                                 rf-get-gamma-&-epsilon binary-numbers)
        ;; [gamma-dec epsilon-dec] 
        res1 (calc-res (eduction (map str/join) gamma&epsilon))
        oxygen&co2 (get-oxygen-&-co2 binary-numbers)
        res2 (calc-res oxygen&co2)]
    [res1 res2]))