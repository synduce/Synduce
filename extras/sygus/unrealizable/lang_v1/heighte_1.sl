(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun f0 ((x Int)) Int
 ((Start Int (Ic x (- Start) (+ Start Start) (max Start Start))) (Ic Int ((Constant Int)))))
(declare-var i Int)
(declare-var i0 Int)
(constraint (= (+ 1 (max i i0)) (f0 i)))
(check-synth)
