(set-logic LIA)
(synth-fun f0 ((x0 Int) (x1 Int)) Int ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x0 x1 (- Ix) (+ Ix Ix))) (Ic Int ((Constant Int)))))
(declare-var i1 Int)
(declare-var i0 Int)
(declare-var i Int)
(constraint (= (+ (+ i i0) i1) (f0 i i0)))
(check-synth)
