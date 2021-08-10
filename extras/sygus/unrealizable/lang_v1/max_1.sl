(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join ((x91 Int)) Int
 ((Start Int (Ic x91 (- Start) (+ Start Start) (max Start Start))) (Ic Int ((Constant Int)))))
(declare-var p Int)
(declare-var i15 Int)
(constraint (or (not (= (ite (> i15 p) i15 0) 0)) (= (max p i15) (join p))))
(check-synth)
