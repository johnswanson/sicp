(ns sicp.metacirc
  (:refer-clojure :exclude [eval apply true? false?]))

(declare
  apply-in-underlying

  eval
  apply

  list-of-values
  eval-if
  eval-sequence
  eval-assignment
  eval-definition

  self-evaluating?

  text-of-quotation

  get-tag

  variable?

  assignment-variable
  assignment-value

  definition-variable
  definition-value

  lambda-parameters
  lambda-body

  make-lambda

  if?
  if-predicate
  if-consequent
  if-alternative

  make-if

  begin-actions
  last-exp?
  first-exp
  rest-exps
  sequence->exp
  make-begin

  application?
  operator
  operands
  no-operands?
  first-operand
  rest-operands

  ; cond?
  ; cond-clauses
  ; cond-else-clause?
  ; cond-predicate clause
  ; cond-actions clause
  ; cond->if
  ; expand-clauses

  true?
  false?

  make-procedure
  compound-procedure?
  procedure-parameters
  procedure-body
  procedure-environment

  enclosing-environment
  first-frame env
  make-frame
  frame-variables
  frame-values
  add-binding-to-frame!
  extend-environment
  lookup-variable-value
  set-variable-value!
  define-variable!

  primitive-procedure?
  primitive-implementation
  primitive-procedures
  primitive-procedure-names
  primitive-procedure-objects
  apply-primitive-procedure


  setup-environment
  input-prompt
  output-prompt
  driver-loop
  prompt-for-input
  announce-output
  user-print)

(def eval-fns
  {'if eval-if
   'begin eval-sequence
   'set! eval-assignment
   'define eval-definition
   'quote (fn [exp _] (text-of-quotation exp))
   ; 'cond (fn [exp env] (eval (cond->if exp) env))
   'lambda (fn [exp env] (make-procedure (lambda-parameters exp)
                                         (lambda-body exp)
                                         env))
   })

(defn error [& args]
  (throw (Throwable. (clojure.core/apply str args))))

(defn apply-in-underlying [f args] (clojure.core/apply f args))

(defn eval [exp env]
  (cond
    (self-evaluating? exp) exp
    (variable? exp) (lookup-variable-value exp env)
    (eval-fns (get-tag exp)) ((eval-fns (get-tag exp)) exp env)
    (application? exp) (apply (eval (operator exp) env)
                              (list-of-values (operands exp) env))
    :else (throw (Throwable. "Unknown expression type"))))

(defn apply [procedure args]
  (cond
    (primitive-procedure? procedure) (apply-primitive-procedure procedure args)
    (compound-procedure? procedure) (eval (procedure-body procedure)
                                          (extend-environment (procedure-parameters procedure)
                                                              args
                                                              (procedure-environment procedure)))
    :else (error (format "Unknown procedure type: %s" procedure))))

(defn list-of-values [exps env]
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(defn eval-if [exp env]
  (if (if-predicate exp)
    (if-consequent exp)
    (if-alternative exp)))

(defn eval-sequence [exps env]
  (cond
    (last-exp? exps) (eval (first-exp exps) env)
    :else (do (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(defn eval-assignment [exp env]
  (set-variable-value! (assignment-variable exp) (assignment-value exp) env))

(defn eval-definition [exp env]
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(defn self-evaluating? [exp]
  (or
    (number? exp)
    (keyword? exp)
    (string? exp)))

;; (quote (x y))
(defn text-of-quotation [exp]
  (second exp))

(defn get-tag [exp]
  (first exp))

(defn variable? [exp] (symbol? exp))

;; (set! x 5)
(defn assignment-variable [exp]
  (second exp))
(defn assignment-value [exp]
  (second (next exp)))

;; (define x 5)
;; (define (sqr x) (* x x))
(defn definition-variable [exp]
  (if (symbol? (second exp))
    (second exp)
    (first (second exp))))
(defn definition-value [exp]
  (println exp)
  (if (symbol? (second exp))
    (first (nnext exp))
    (make-lambda (next (second exp))
                 (first (nnext exp)))))

;; (lambda (x y) (+ x y))
(defn lambda-parameters [exp]
  (second exp))
(defn lambda-body [exp]
  (first (nnext exp)))

(defn make-lambda [parameters body]
  (cons 'lambda (list parameters body)))

;; (if (pred?) (consequent) (alternative))
(defn if-predicate [exp]
  (nth exp 1 nil))
(defn if-consequent [exp]
  (nth exp 2 nil))
(defn if-alternative [exp]
  (nth exp 3 nil))

(defn make-if [predicate consequent alternative]
  (list 'if predicate consequent alternative))

(defn begin-actions [exp] (next exp))
(defn last-exp? [coll] (not (seq (next coll))))
(defn first-exp [coll] (first coll))
(defn rest-exps [coll] (next coll))
(defn sequence->exp [coll]
  (cond
    (empty? coll) coll
    (last-exp? coll) (first-exp coll)
    :else (make-begin coll)))
(defn make-begin [coll] (cons 'begin coll))

(defn application? [exp]
  (list? exp))

(defn operator [exp]
  (first exp))
(defn operands [exp]
  (next exp))

(defn no-operands? [ops] (empty? ops))
(defn first-operand [ops] (first ops))
(defn rest-operands [ops] (next ops))

(defn true? [s] (clojure.core/true? s))
(defn false? [s] (clojure.core/false? s))

;; (procedure (x y) (+ x y) env)
(defn make-procedure [parameters body env]
  (list 'procedure parameters body env))
(defn compound-procedure? [procedure]
  (= (get-tag procedure) 'procedure))
(defn procedure-parameters [procedure]
  (nth procedure 1))
(defn procedure-body [procedure]
  (nth procedure 2))
(defn procedure-environment [procedure]
  (nth procedure 3))

(defn enclosing-environment [env]
  (next env))
(defn first-frame [env]
  (first env))

(defn make-frame [variables values]
  (atom (clojure.core/apply assoc {} (interleave variables values))))
(defn frame-variables [frame]
  (keys @frame))
(defn frame-values [frame]
  (vals @frame))
(defn add-binding-to-frame! [variable value frame]
  (swap! frame assoc variable value))
(defn define-in-frame! [variable value frame]
  (swap! frame assoc variable value))

(defn extend-environment [variables values base-environment]
  (if (= (count variables) (count values))
    (cons (make-frame variables values) base-environment)
    (if (< (count variables) (count values))
      (error "Too many arguments supplied" variables values)
      (error "Too few arguments supplied" variables values))))

(defn lookup-variable-value [variable env]
  (if-let [value (first
                   (filter #(not (nil? %))
                           (map #(get @% variable nil) env)))]
    value
    (error "Unbound variable -- SET!" variable)))

(defn define-variable! [variable value env]
  (let [frame (first-frame env)]
    (define-in-frame! variable value frame)))

(def the-empty-environment [])

(defn setup-environment []
  (let [initial-env (extend-environment (primitive-procedure-names)
                                        (primitive-procedure-objects)
                                        the-empty-environment)]
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(defn primitive-procedure? [proc]
  (= (get-tag proc) 'primitive))

(defn primitive-implementation [proc] (second proc))

(def primitive-procedures
  (list (list 'car first)
        (list 'cdr next)
        (list 'cons cons)
        (list 'null? nil?)
        (list '+ +)
        (list '- -)
        (list '/ /)
        (list '* *)
        ))

(defn primitive-procedure-names []
  (map first primitive-procedures))

(defn primitive-procedure-objects []
  (map (fn [proc] (list 'primitive (second proc)))
       primitive-procedures))

(defn apply-primitive-procedure [proc args]
  (apply-in-underlying (primitive-implementation proc) args))

(def input-prompt ";;; M-Eval input:")
(def output-prompt ";;; M-Eval value:")

(def the-global-environment
  (setup-environment))

(defn driver-loop []
  (prompt-for-input input-prompt)
  (let [input (read)]
    (let [output (eval input the-global-environment)]
      (announce-output output-prompt)
      (println output)))
  (driver-loop))

(defn prompt-for-input [s]
  (newline) (newline) (println s))

(defn announce-output [s]
  (newline) (println s))

(defn user-print [obj]
  (if (compound-procedure? obj)
    (println (list 'compound-procedure
                   (procedure-parameters obj)
                   (procedure-body obj)
                   '<procedure-env>))
    (println obj)))



