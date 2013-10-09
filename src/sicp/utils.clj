(ns sicp.utils)

(defn =number?
  "X is both a number and equal to a value"
  [x n]
  (and (number? x)
       (= x n)))
