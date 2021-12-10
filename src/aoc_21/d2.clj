(ns aoc-21.d2
  (:require [aoc-21.base :as b]))

(def command->direction-matrix
  {"forward" [1 0]
   "up" [0 -1]
   "down" [0 1]})

(def re-find-groups
  (comp next re-find))

(defn multiply-matrix
  [matrix n]
  (mapv #(* % n) matrix))

(defn rf-add-matrix
  ([] [0 0])
  ([acc] acc)
  ([acc mat]
   (mapv + acc mat)))

(defn updates-list
  [coll fs]
  (let [fs (concat fs (repeat identity))]
    (map #(%1 %2) fs coll)))

(defn get-res1
  [command&n-tuples]
  (let [xf-command->matrix (comp (map #(updates-list % ;;broken, needs to be a list update (using map, probably should extract to own fn)
                                                     [command->direction-matrix
                                                      b/parse-int])) 
                            ;; (map #(vector (command->direction-matrix (first %))
                            ;;               (b/parse-int (second %))))
                                 (map #(apply multiply-matrix %)))]
    (transduce xf-command->matrix rf-add-matrix command&n-tuples)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn aim
  [dir-fn sub-state n]
  (update sub-state :aim + (dir-fn n)))

(defn move
  [{:keys [aim], :as sub-state} n]
  (let [move-matrix [n (* n aim)]]
    (update sub-state :position rf-add-matrix move-matrix)))

(def command->fn
  {"forward" move
   "up" (partial aim -)
   "down" (partial aim +)})

(defn rf-sub
  ([] {:position [0 0]
       :aim 0})
  ([sub] (:position sub))
  ([sub [f n]]
   (f sub n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -main
  []
  (let [command-lines (b/get-split-input 2)
        xf-split-command-n (map #(re-find-groups #"(\w++) (\d++)" %))
        command&n-tuples (eduction xf-split-command-n command-lines)
        
        result1 (get-res1 command&n-tuples)
        
        xf2 (comp  xf-split-command-n
                  (map #(vector (command->fn (first %))
                                (b/parse-int (second %)))))
        result2 (transduce xf2 rf-sub command-lines)]
    (mapv #(apply * %) [result1 result2])))