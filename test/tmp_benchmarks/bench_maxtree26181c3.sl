(set-logic LIA)
(synth-fun join ((x1 Int) (x2 Int) (x3 Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic x1 x2 x3 (- Ix) (+ Ix Ix))) (Ic Int ((Constant Int)))))
(check-synth)
