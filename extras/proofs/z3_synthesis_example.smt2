(set-option :print-success true)
(set-logic UFLIA)
(set-option :produce-models true)
(declare-fun f2 (Int Int Int) Int)
(assert
 (forall ((i0 Int) (x Int) (y Int))
  (and (= (- x y) (+ x (f2 x y i0))))))
(check-sat)
(get-model)
