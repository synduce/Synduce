(set-logic LIA)
(synth-fun add1 ((x54 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x54 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun add ((x55 Int) (x56 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x55 x56 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(constraint (= 0 (add 0 0)))
(constraint (= 1 (add (add1 0) 0)))
(constraint (= (- 1) (add 1 0)))
(constraint (= 1 (add 0 (add1 0))))
(constraint (= 2 (add (add1 (add1 0)) 0)))
(check-synth)
