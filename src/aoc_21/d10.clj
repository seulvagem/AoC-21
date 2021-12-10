(ns aoc-21.d10
  (:require [aoc-21.base :as b]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [medley.core :as m]))


(def openings
  #{\( \[ \{ \<})

(def closings
  #{\) \] \} \>})

(def opens->closes
  {\( \)
   \[ \]
   \{ \}
   \< \>})

(def closes->points
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(def opens->points
  {\( 1
   \[ 2
   \{ 3
   \< 4})

(defn parse-line
  [line]
  (reduce (fn [stack c]
            (if (openings c)
              (conj stack c)
              (if (= c (opens->closes (peek stack)))
                (pop stack)
                (reduced c)))) [] line))

(defn get-corrupted
  [line]
  (let [res (parse-line line)]
    (when (char? res)
      res)))

(defn count-res2-points
  [opens]
  (reduce (fn [points c]
            (-> points
                (* 5)
                (+ (opens->points c)))) 0 opens))

(defn main
  [input-lines]
  (let [xf-res1 (comp (map get-corrupted)
                      (filter identity)
                      (map closes->points))
        res1 (transduce xf-res1 + input-lines)
        xf-res2 (comp (map parse-line)
                      (filter coll?)
                      (map reverse)
                      (map count-res2-points))
        ;; res2 (transduce xf-res2 + input-lines)
        res2-points (sort (into [] xf-res2 input-lines))
        middle (-> res2-points count (/ 2) int)
        res2 (nth res2-points middle)]
    [res1 res2]))


(def simple-input
  "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")

(defn -main
  []
  (main (b/get-split-input 10)))