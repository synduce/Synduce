(set-logic LIA)
(synth-fun f2 ((x7 Int)) Int ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((- Ix Ix))) (Ix Int (Ic x7 (- Ix) (+ Ix Ix))) (Ic Int ((Constant Int)))))
(declare-var i1502 Int)
(declare-var i1501 Int)
(declare-var i0 Int)
(constraint (= (- i1501 i1502) (f2 i0)))
(check-synth)
