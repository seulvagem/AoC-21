(ns aoc-21.d5
  (:require [aoc-21.base :as b]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]))

;; 503,977 -> 843,637
(defn tuple->point
  [[x y]]
  {:x x, :y y})

(defn parse-line
  [line-str]
  (let [points-str (str/split line-str #" -> ")
        xf (comp (map (partial b/re-find-groups #"(\d+),(\d+)"))
                 (map #(map b/parse-int %))
                ;;  (map vec)
                 (map tuple->point))]
    (into [] xf points-str)))


(s/def ::coord-val nat-int?)

(s/def ::x ::coord-val)

(s/def ::y ::coord-val)
(s/def ::point (s/keys :req-un [::x ::y]))

(s/def ::line (s/every ::point
                       :count 2
                       :kind vector?
                       :distinct true))

(def axis-set #{:x :y})

(s/def ::axis axis-set)

(s/fdef line-parallel-to-axis?
  :args (s/cat :axis-key ::axis
               :line ::line)
  :ret boolean?)

(defn- line-parallel-to-axis?
  "takes an axis key and a line [a b], checks if the values are equal"
  [axis-key line]
  (apply = (map axis-key line)))

(s/fdef vertical?
  :args (s/cat :line ::line)
  :ret boolean?)

(defn vertical?
  [line]
  (line-parallel-to-axis? :x line))

(s/fdef horizontal?
  :args (s/cat :line ::line)
  :ret boolean?)

(defn horizontal?
  [line]
  (line-parallel-to-axis? :y line))

(s/fdef explode-line
  :args (s/cat :line ::line)
  :ret (s/every ::point))

(defn rf-points->map
  ([] {})
  ([m] m)
  ([m point]
   (update m point (fnil inc 0))))

(defn explode-line ;;needs if diagonal?
  "takes 2 right-angled points a and b, returns every point between them"
  [[fix-axis ranged-axis] [a :as line]]
  (let [[start end] (sort (map ranged-axis line))
        fix-val (fix-axis a)
        base-point {fix-axis fix-val}]
    (map #(assoc base-point ranged-axis %) (range start (inc end)))))

(defn explode-diagonal 
  [[{xa :x ya :y} {xb :x yb :y} :as line]]
  (let [greater->step {true -1
                       false 1}
        x-step (greater->step (> xa xb))
        y-step (greater->step (> ya yb))]
    (map (fn [x y] {:x x :y y})
         (range xa (+ xb x-step) x-step)
         (range ya (+ yb y-step) y-step))))

(defn straight?
  [line]
  (or (horizontal? line)
      (vertical? line)))

(s/def ::line-category #{:horizontal :vertical :diagonal})

(s/fdef line-category
  :args (s/cat :line ::line)
  :ret ::line-category)

(s/fdef diagonal?
  :args (s/cat :line ::line)
  :ret boolean?)

(defn diagonal? ;; only straight angles and 45º, doesn't accept two-step differences ({:x 0 :y 0} - {:x 4 :y 2})
  [line]
  (let [axis-diff (fn [line axis]
                    (b/abs (apply - (map #(axis %) line))))
        diffs (map (partial axis-diff line) axis-set)]
    (apply = diffs)))

(s/fdef line-category
  :args (s/cat :line ::line)
  :ret keyword?)

(defn line-category
  [line]
  (cond
    (horizontal? line) :horizontal
    (vertical? line) :vertical
    (diagonal? line) :diagonal
    :else :THIS-SHOULD-NOT-BE-HERE))

(s/fdef explode-line
  :args (s/cat
         :type ::line-category
         :line ::line)
  :ret (s/every ::point :min-count 2))

(def category->fn
  {:horizontal (partial explode-line [:y :x])
   :vertical (partial explode-line [:x :y])
   :diagonal explode-diagonal})

(def xf-explode-line
  (comp (map (juxt line-category identity))
        (map #(b/updates-list % [category->fn]))
        (mapcat b/call)))

(defn -main
  []
  (let [str-lines (b/get-split-input 5)
        xf-points (comp (map parse-line)
                ;;  (filter straight?)
                        xf-explode-line)
        board (transduce xf-points rf-points->map str-lines)
        xf-count (comp (map val)
                       (filter #(> % 1)))]
    (transduce xf-count b/rf-count board)
    ;; (count (into []
    ;;              xf-count
    ;;              board))
    ;; (take 10 (sequence xf str-lines))
    ))

(stest/instrument)
