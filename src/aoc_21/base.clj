(ns aoc-21.base
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clj-queue.core :as q]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

(defn get-input
  "slurps the input from the corresponding day"
  [day]
  (-> (str "input/d" day ".txt")
      io/resource
      ;; io/file
      slurp))

(def get-split-input
  (comp str/split-lines get-input))

(defn parse-char [s]
  (nth s 0 nil))

(defn parse-int
  ([s]
   (parse-int s 10))
  ([s rdx]
   (try (Integer/parseInt s rdx)
        (catch Exception e (println e) nil))))

(defn upd-if-not
  "takes a new value, returns a fn that expects an old value, it will return old unless it is falsy, returns new then"
  [new]
  #(if-not %
    new
    %))

(defn exercise-first
  [& args]
  (map first (apply clojure.spec.alpha/exercise args)))

(defmacro couple-times
  [times-mult op-mult op]
  `(dotimes [_# ~times-mult]
     (time (dotimes [_# ~op-mult]
             ~op))))

(defmacro sdef-with-gen
  ([s-id s-form upd-gen-fn]
   `(let [s-form# ~s-form]
      (clojure.spec.alpha/def
        ~s-id (clojure.spec.alpha/spec
               s-form#
               :gen  #(clojure.spec.gen.alpha/fmap
                       ~upd-gen-fn
                       (clojure.spec.alpha/gen s-form#)))))))

(defn- queue-conj-up-to
  [q x lim]
  (let [nq (conj q x)]
    (if (> (count nq) lim)
      (pop nq)
      nq)))

(defn get-xf-partition
  "like partition, but as a transducer. step defaults to 1"
  ([length]
   (get-xf-partition length 1))
  ([length step]
   (fn [rf]
     (let [vseg (volatile! (q/ueue))
           vstep-count (volatile! -1)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
            (let [seg (vswap! vseg queue-conj-up-to input length)
                  seg-length (count seg)]
              (if (and (>= seg-length length)
                       (zero? (rem (vswap! vstep-count inc) step)))
                (rf result (seq seg))
                result))))))))

(defn updates
  "updates an associative or indexed coll with several key - fn pairs"
  [m & kfs]
  (transduce (get-xf-partition 2 2)
              (fn ([m] m) 
                ([m [k f]]
                   (update m k f))) m kfs))

(defn call
  ([[f args]]
   (call f args))
  ([f args]
   (f args)))


(defn updates-list
  [coll f-coll]
  (let [infinite-f-coll (concat f-coll (repeat identity))]
    (map call infinite-f-coll coll)))

(defn get-parse-fn
  [length]
  (condp > length 
   10 #(Integer/parseInt %)
   19 #(Long/parseLong %)
   ##Inf bigint))

(defn parse-num
  [num-str]
  (let [parse-fn (get-parse-fn (count num-str))]
    (parse-fn num-str)))

(defn char->int
  [c]
  (Character/digit c 10))

(defn most
  "Return the most argument (as defined by the compare function) in O(n) time."
  {:arglists '([& xs])}
  ([] nil)
  ([a] a)
  ([a b] (if (neg? (compare a b)) b a))
  ([a b & more] (reduce most (most a b) more)))

;; (defn juxt-args
;;   "kinda like juxt, but the returned function expects the same number of args as the function, applies each arg individually"
;;   [& fns]
;;   (fn [& args] 
;;     (mapv #(%1 %2) fns args)))

(defn rf-first
  "reducing function that returns the first item (useful on tranducers)"
  ([] (rf-first identity))
  ([cf]
   (completing (fn [_ x]
                 (reduced x))
               cf)))

(defn rf-count
  ([] 0)
  ([count] count)
  ([count _]
   (inc count)))

(defn re-find-groups
  [& re-find-args]
  (next (apply re-find re-find-args)))

(defn abs
  [n]
  (if (neg? n)
    (- n)
    n))

(def xf-println
  (map (fn [x]
         (println x)
         x)))

(def get-rf-count-map
  (partial completing
           (fn ([] {})
             ([m n] (update m n (fnil inc 0))))))

(defn get-adjacent-coords
  [[x y]]
  ;; (map #(u/evolve % coord) adj-fns)
  (list [x (dec y)]
        [(inc x) y]
        [x (inc y)]
        [(dec x) y]))

(defn parse-grid-str->matrix
  ([string]
   (parse-grid-str->matrix string identity))
  ([f string]
   (let [lines (str/split-lines string)]
     (into [] (map #(mapv f %)) lines))))

(defn parse-grid-str->map
  ([string]
   (parse-grid-str->map identity {} string))
  ([f string]
   (parse-grid-str->map f {} string))
  ([f m string]
   (let [lines (str/split-lines string)
         height (count lines)
         length (count (first lines))
         coords (for [y (range height)
                      x (range length)]
                  [x y])
         get-val #(f (get-in lines (reverse %)))]
     (transduce (map (juxt identity get-val)) conj m coords))))


;; combinations

(defn- combinations-of-1
  "kinda stupid, but it keeps combinate DRY!"
  [coll]
  (map hash-set coll))

(defn- combinate
  [f]
  (fn [coll]
    (loop [acc #{}
           seq1 (seq coll)]
      (let [x (first seq1)
            seq2 (next seq1)]
        (if-not seq2
          acc
          (let [xf (map #(conj % x))
                nacc (into acc xf (f seq2))]
            (recur nacc seq2)))))))

(defn- get-combination-fn
  [n]
  (nth (iterate combinate combinations-of-1) (dec n)))

(defn combinations-of
  [n coll]
  (let [c-fn (get-combination-fn n)]
    (c-fn coll)))

(defn combinations-up-to
  [n coll]
  (let [xf-combs-of (map #(combinations-of % coll))]
    (transduce xf-combs-of set/union (range 1 (inc n)))))

(defn all-combinations
  [coll]
  (combinations-up-to (dec (count coll)) coll))

(defn comp-rev
  "comps the reverse of fns, should not be used in any kind of partial application context"
  [& fns]
  (comp (reverse fns)))