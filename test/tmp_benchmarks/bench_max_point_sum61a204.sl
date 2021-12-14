(set-logic LIA)
(synth-fun s1 ((x3 Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic x3 (- Ix) (+ Ix Ix))) (Ic Int ((Constant Int)))))
(check-synth)
