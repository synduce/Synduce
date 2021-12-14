(set-logic LIA)
(synth-fun inner1 ((x24 Int) (x25 Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic x24 x25 (- Ix) (+ Ix Ix))) (Ic Int ((Constant Int)))))
(check-synth)
