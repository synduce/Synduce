(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join ((x1 Int) (x2 Int)) Int ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((max Ix Ix))) (Ix Int (Ic x1 x2 (- Ix) (+ Ix Ix) (max Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i0 Int)
(declare-var i1 Int)
(constraint (= i1 (join 0 i1)))
(constraint (= (max (+ i1 i0) 0) (join (max (+ 0 i0) 0) i1)))
(check-synth)
