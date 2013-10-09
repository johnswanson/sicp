(ns sicp.diff "Symbolic differentiation"
  (:require [sicp.utils :refer [=number?]]))

(defn variable?
  "Is x a variable?"
  [x]
  (symbol? x))

(defn same-variable?
  "Are x and y the same variable?"
  [x y]
  (and
    (variable? x)
    (variable? y)
    (= x y)))

(defn sum?
  "Is e a sum?"
  [e]
  (= (first e) '+))

(defn addend
  "The addend of a sum"
  [e]
  (second e))

(defn augend
  "The augend of a sum"
  [e]
  (nth e 2))

(defn make-sum
  "The resulting sum"
  [m1 m2]
  (cond
    (=number? m1 0) m2
    (=number? m2 0) m1
    :else (list '+ m1 m2)))

(defn product?
  "Is e a product?"
  [e]
  (= (first e) '*))

(defn multiplier
  "The multiplier of a product"
  [e]
  (second e))

(defn multiplicand
  "The multiplicand of a product"
  [e]
  (nth e 2))

(defn make-product
  [m1 m2]
  "The result of a multiplication"
  (cond
    (or
      (=number? m1 0)
      (=number? m2 0)) 0
    (=number? m1 1) m2
    (=number? m2 1) m1
    :else (list '* m1 m2)))

(defn exponentiation?
  "The expression is an exponentiation"
  [e]
  (= (first e) '**))

(defn base
  "The basis for the exponentiation"
  [e]
  (second e))

(defn exponent
  "The exponent of the exponentiation."
  [e]
  (nth e 2))

(defn make-exponentiation
  "Builds an exponentiation expression"
  [m1 m2]
  (cond
    (=number? m2 0) 1
    (=number? m2 1) m1
    :else (list '** m1 m2)))

;; deriv of constant = 0
;; deriv of variable wrt that variable = 1
;; deriv of variable wrt another variable = other variable

(defn deriv [expr wrt]
  (cond
    (number? expr) 0
    (same-variable? expr wrt) 1
    (variable? expr) expr
    (sum? expr) (make-sum
                  (deriv (addend expr) wrt)
                  (deriv (augend expr) wrt))
    (product? expr) (make-sum
                      (make-product (multiplier expr)
                                    (deriv (multiplicand expr) wrt))
                      (make-product (multiplicand expr)
                                    (deriv (multiplier expr) wrt)))
    (exponentiation? expr) (make-product
                             (exponent expr)
                             (make-product
                               (make-exponentiation
                                 (base expr)
                                 (dec (exponent expr)))
                               (deriv (base expr) wrt)))
    :else (throw (Throwable. "ERROR: Unknown type: " expr))))

