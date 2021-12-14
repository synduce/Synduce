(set-logic LIA)
(synth-fun j2 ((x0 Int) (x1 Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic x0 x1 (- Ix) (+ Ix Ix))) (Ic Int ((Constant Int)))))
(check-synth)
