(set-logic LIA)
(synth-fun f2 ((x1 Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic x1 (- Ix) (+ Ix Ix))) (Ic Int ((Constant Int)))))
(synth-fun f1 ((x2 Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic x2 (- Ix) (+ Ix Ix))) (Ic Int ((Constant Int)))))
(declare-var i Int)
(declare-var i0 Int)
(constraint (= i0 (+ (+ (f1 0) (f2 i0)) 0)))
(constraint (= (+ i i0) (+ (+ (f1 (+ i 0)) (f2 i0)) 0)))
(check-synth)
