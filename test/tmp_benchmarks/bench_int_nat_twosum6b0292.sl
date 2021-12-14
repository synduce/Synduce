(set-logic LIA)
(synth-fun add1 ((x44 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x44 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun add ((x45 Int) (x46 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x45 x46 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun c0 () Int)
(constraint (= 0 (add c0 c0)))
(constraint (= 1 (add (add1 c0) c0)))
(constraint (= (- 1) (add 1 c0)))
(constraint (= 1 (add c0 (add1 c0))))
(check-synth)
