(ns aoc-21.base
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clj-queue.core :as q]))

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

(defn xf-partition
  "like partition, but as a transducer. step defaults to 1"
  ([length]
   (xf-partition length 1))
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
  (transduce (xf-partition 2 2)
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