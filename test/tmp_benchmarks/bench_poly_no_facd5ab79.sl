(set-logic NIA)
(synth-fun odot ((x2 Int) (x3 Int)) Int ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x2 x3 (- Ix) (+ Ix Ix) (* Ix Ix) (div Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i Int)
(declare-var x Int)
(declare-var i0 Int)
(constraint (= i0 (odot 0 i0)))
(constraint (= (+ (* x i0) i) (odot (+ (* x 0) i) i0)))
(check-synth)
