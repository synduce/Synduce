(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun s1 ((x1 Int)) Int ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((max Ix Ix))) (Ix Int (Ic x1 (- Ix) (+ Ix Ix) (max Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i0 Int)
(declare-var i Int)
(constraint (= (max i i0) (s1 i)))
(check-synth)
