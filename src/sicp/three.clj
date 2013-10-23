(ns sicp.three
  (:require [clojure.math.numeric-tower :refer [gcd sqrt abs]]))

(defn make-monitored [f]
  (let [a (atom 0)]
    (fn [& args]
      (swap! a inc)
      (println "Calling " f)
      (apply f args))))

(defn make-monitored-sym [f]
  (let [a (atom 0)]
    (fn [s & args]
      (if (= s 'how-many-calls?)
        @a
        (do
          (swap! a inc)
          (apply f s args))))))

(defn cesaro-test []
  (= (gcd (rand-int 10000000) (rand-int 10000000)) 1))

(defn monte-carlo [trials experiment]
  (let [res (repeatedly trials #(if (experiment) 1 0))]
    (/ (reduce + res) (count res))))

(defn estimate-pi [trials]
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(defn integral-test [pred? x1 x2 y1 y2]
  (let [[x-min x-max] ((juxt min max) x1 x2)
        [y-min y-max] ((juxt min max) y1 y2)
        height (- y-max y-min)
        width (- x-max x-min)]
    (fn []
      (let [x (+ (rand width) x-min)
            y (+ (rand height) y-min)]
        (pred? x y)))))

(defn area [x1 x2 y1 y2]
  (* (abs (- x1 x2)) (abs (- y1 y2))))

(defn estimate-integral [pred? x1 x2 y1 y2 trials]
  (double (* (area x1 x2 y1 y2)
             (monte-carlo trials (integral-test pred? x1 x2 y1 y2)))))

