(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join2 ((x14 Int) (x15 Int) (x16 Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic x14 x15 x16 (- Ix) (+ Ix Ix) (max Ix Ix))) (Ic Int ((Constant Int)))))
(declare-var i2 Int)
(declare-var i0 Int)
(declare-var p7 Int)
(declare-var p Int)
(constraint (= (max p i2) (max p (join2 p7 i0 i2))))
(check-synth)
