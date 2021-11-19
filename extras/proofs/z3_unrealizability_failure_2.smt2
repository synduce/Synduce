(set-option :print-success true)
(set-logic UFLIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(set-option :produce-models true)

(declare-fun f3 (Int Int) Int)
(declare-fun f2 (Int Int) Int)
(declare-fun f1 (Int Int) Int)

(assert
 (forall ((i0 Int) (x Int) (y Int))
  (and
    (= (- x y 1)
    (+ i0 (f2 0 y) (f1 (max y 0) 0) (f2 x i0))))))

(check-sat)
