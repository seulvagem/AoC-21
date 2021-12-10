(ns aoc-21.d8
  (:require [aoc-21.base :as b]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [medley.core :as m]))

(def simple-input "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |fgae cfgab fg bagce")

(def unique-counts
  #{2 3 4 7})

(defn main
  [input-lines]
  (let [xf (comp (map #(str/split % #" \|"))
                 (map second)
                 (mapcat #(str/split % #" "))
                 (map count)
                 (filter unique-counts))]
    (transduce xf b/rf-count input-lines)))

(defn -main
  []
  (main (b/get-split-input 8)))

(def segm-count->digits
  {2 [1]
   3 [7]
   4 [4]
   5 [2 3 5]
   6 [0 6 9]
   7 [8]})

(def digits->segms
  {1 [:c :f]
   2 [:a :c :d :e :g]
   3 [:a :c :d :f :g]
   4 [:b :c :d :f]
   5 [:a :b :c :e :f :g]
   6 [:a :b :d :e :f :g]
   7 [:a :c :f]
   8 [:a :b :c :d :e :f :g]
   9 [:a :b :c :d :f :g]})