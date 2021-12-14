(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun f0 ((x0 Int) (x1 Int)) Int ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x0 x1 (- Ix) (+ Ix Ix) (max Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i0 Int)
(declare-var i Int)
(declare-var p Int)
(constraint (= (+ 1 (max i i0)) (f0 p i)))
(check-synth)
