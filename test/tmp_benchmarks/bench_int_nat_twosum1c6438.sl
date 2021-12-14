(set-logic LIA)
(synth-fun sub1 ((x64 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x64 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun add1 ((x65 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x65 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun add ((x66 Int) (x67 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x66 x67 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(constraint (= 0 (add 1 1)))
(constraint (= 1 (add (add1 1) 1)))
(constraint (= (- 1) (add (sub1 1) 1)))
(constraint (= 1 (add 1 (add1 1))))
(constraint (= 2 (add (add1 (add1 1)) 1)))
(constraint (= 0 (add (sub1 (add1 1)) 1)))
(check-synth)
