(ns sicp.diff-test
  (:require [clojure.test :refer :all]
            [sicp.diff :refer :all]))

(def variables '(x y z))
(def non-variables '("x" 3 [x]))
(def same-variables '(x x))
(def different-variables '(x y))

(def a-sum '(+ x y))
(def a-product '(* x y))

(deftest basics
  (testing "variables are variables."
    (is (every? variable? variables)))
  (testing "non-variables are not variables."
    (is (not-any? variable? non-variables)))
  (testing "same-variables are the same"
    (is (apply same-variable? same-variables)))
  (testing "Are 'x and 'y the same variable?"
    (is (not (apply same-variable? different-variables))))
  (testing "Is a sum a sum?"
    (is (sum? a-sum)))
  (testing "Is a product a sum?"
    (is (not (sum? a-product))))
  (testing "The addend of the example is x"
    (is (= (addend a-sum) 'x)))
  (testing "The augend of the example is y"
    (is (= (augend a-sum) 'y)))
  (testing "Is a product a product?"
    (is (product? a-product)))
  (testing "Is a product a sum?"
    (is (not (sum? a-product))))
  (testing "The multiplier of the product is x"
    (is (= 'x (multiplier a-product))))
  (testing "The multiplicand of the product is y"
    (is (= 'y (multiplicand a-product)))))

