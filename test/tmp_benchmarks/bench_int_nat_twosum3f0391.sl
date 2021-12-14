(set-logic LIA)
(synth-fun sub1 ((x60 Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic x60 (- Ix) (+ Ix Ix))) (Ic Int ((Constant Int)))))
(synth-fun add1 ((x61 Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic x61 (- Ix) (+ Ix Ix))) (Ic Int ((Constant Int)))))
(synth-fun add ((x62 Int) (x63 Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic x62 x63 (- Ix) (+ Ix Ix))) (Ic Int ((Constant Int)))))
(synth-fun c0 () Int)
(constraint (= 0 (add c0 c0)))
(constraint (= 1 (add (add1 c0) c0)))
(constraint (= (- 1) (add (sub1 c0) c0)))
(constraint (= 1 (add c0 (add1 c0))))
(constraint (= 2 (add (add1 (add1 c0)) c0)))
(constraint (= 0 (add (sub1 (add1 c0)) c0)))
(check-synth)
