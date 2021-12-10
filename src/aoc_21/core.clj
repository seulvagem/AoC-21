(ns aoc-21.core)

(defmacro run-print-day
  [d]
  `(do
     (println ~(str "\nday " d ":"))
     (println (~(symbol (str "d" d) "-main")))))

(defmacro doseq-macro
  [macroname & args]
  `(do
     ~@(map (fn [arg] (list macroname arg)) args)))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
