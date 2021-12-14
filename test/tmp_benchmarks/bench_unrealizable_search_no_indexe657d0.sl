(set-logic LIA)
(synth-fun f ((x12 Int) (x13 Int) (x14 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix))) (Ix Int (Ic x12 x13 x14 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i1 Int)
(declare-var i0 Int)
(declare-var x Int)
(constraint (= (ite (= x i0) 0 0) (f x i0 0)))
(constraint (= (ite (= x i0) 1 (ite (= x i1) 0 0)) (f x i0 (ite (= x i1) 0 0))))
(check-synth)
