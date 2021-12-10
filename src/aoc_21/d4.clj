(ns aoc-21.d4
  (:require [aoc-21.base :as b]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]))

(def board-size 5)

(def n-set (into #{} (map str) (range 100)))
(s/def ::n n-set)

(s/def ::ns (s/with-gen (s/every ::n :distinct true)
              #(gen/shuffle n-set)))

(s/def ::coord-1 (s/int-in 0 board-size))

(s/def ::x ::coord-1)
(s/def ::y ::coord-1)

(s/def ::coord (s/tuple ::y ::x))

(s/def ::coords (s/every ::coord))
(s/def ::marks ::coords)

(s/def ::board-cell (s/or :unmarked ::n
                          :marked nil?)) ;;nil if marked

(s/def ::board-line (s/every ::board-cell :kind vector? :count board-size))

(s/def ::board (s/every ::board-line :kind vector? :count board-size))
(s/def ::boards (s/every ::board))

(s/def ::marked-board (s/keys :req-un [::board ::marks]))

(s/def ::score nat-int?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef mark-board
  :args (s/cat :board ::board
               :n ::n)
  :ret ::marked-board)

(defn mark-board
  [board n]
  (let [coord-range (range (count board))
        coords (for [y coord-range
                     x coord-range]
                 [y x])]
    (reduce (fn [acc coord]
              (if (= (get-in board coord) n)
                (b/updates acc
                           :board #(assoc-in % coord nil)
                           :marks #(conj % coord))
                acc))
            {:board board, :marks []} coords)))

(s/fdef get-row&column
  :args (s/cat :coord ::coord)
  :ret (s/tuple ::coords ::coords))

(defn get-row&column
  [[y x]]
  (let [row (map vector (repeat board-size y) (range board-size))
        column (map vector (range board-size) (repeat board-size x))]
    [row column]))


(s/fdef bingo-line?
  :args (s/cat :line ::board-line)
  :ret boolean?)

(defn bingo-line?
  [line]
  (every? nil? line))

(s/fdef bingo?
  :args (s/cat :marked-board ::marked-board)
  :ret boolean?)

(defn bingo?
  [{:keys [board marks]}]
  (let [get-in-board (partial get-in board)
        coords->cells #(mapv get-in-board %)
        xf (comp (mapcat get-row&column)
                 (map coords->cells)
                  ;;  (map #(do (println %) %))
                 (filter bingo-line?))]
    (transduce xf (b/rf-first boolean) false marks)
      ;; (into [] xf marks)
    ))

(s/fdef get-score
  :args (s/cat :board ::board
               :n ::n)
  :ret ::score)

(defn get-score
  [board n]
  (let [xf (comp cat
                 (filter identity)
                 (map b/parse-int))]
    (transduce xf (completing + #(* % (b/parse-int n))) board)))

;; (defn rf-check-bingo
;;   ([])
;;   ([acc])
;;   ([acc {:keys [found-coords board], :as marked-board}]
;;    (reduce (fn [_ coord]
;;              (let [[row column] (mapv (fn [coords]
;;                                       (map #(get-in board %) coords))
;;                                     (get-row&column coord))]
;;                (when (not (and (some some? row)
;;                                (some some? column)))
;;                  (reduced ))))
;;            nil
;;            coords)))

(s/fdef bingo-round
  :args (s/cat :boards ::boards
               :n ::n)
  :ret (s/or :boards ::boards
             :score ::score))

(defn bingo-round
  [boards n]
  (transduce (map #(mark-board % n))
             (completing (fn [boards {:keys [board], :as marked-board}]
                           (if (bingo? marked-board)
                             (reduced (ensure-reduced (get-score board n)))
                             (conj boards board))))
             [] boards))

(s/fdef bingo-round2
  :args (s/cat :boards ::boards
               :n ::n)
  :ret (s/or :boards ::boards
             :score ::score))

(defn bingo-round2
  [boards n]
  (transduce (map #(mark-board % n))
             
             (fn ([boards {:keys [board], :as marked-board}]
                  (if (bingo? marked-board)
                    (conj boards (get-score board n))
                            ;;  (reduced (ensure-reduced (get-score board n)))
                    ;; boards
                    (conj boards board)))
               ([boards]
                (let [remaining-boards (filter coll? boards)]
                  ;; (println (count remaining-boards)
                  ;;          boards)
                  (if (empty? remaining-boards)
                    (ensure-reduced (last boards))
                    remaining-boards))))
             [] boards))

(s/fdef bingo1
  :args (s/cat :ns ::ns
               :boards ::boards)
  :ret ::score)

(defn bingo1
  [ns boards]
  (reduce (completing bingo-round) boards ns))

(defn bingo2
  [ns boards]
  (reduce (completing bingo-round2) 
  ;;  (fn [boards n]
  ;;           ;; (println (count boards) n)
  ;;           (let [n-boards (bingo-round2 boards n)]
  ;;             (if (empty? n-boards)
  ;;               (reduced (get-score (last boards) n))
  ;;               n-boards)))
          boards ns))

(defn parse-boards
  [str-boards]
  (into []
        (comp (map str/split-lines)
              (map (partial mapv #(vec (re-seq #"\d+" %)))))
        str-boards))

(defn main
  [input]
  (let [split-input (str/split input #"\R{2}")
        [[nstr] str-boards] (split-at 1 split-input)
        ns (str/split nstr #",")
        boards (parse-boards str-boards)
        res1 (bingo1 ns boards)
        res2 (bingo2 ns boards)]
    [res1 res2]))

(defn -main
  []
  (main (b/get-input 4)))

