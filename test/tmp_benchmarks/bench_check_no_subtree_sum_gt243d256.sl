(set-logic LIA)
(synth-fun xi_2 ((x24 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x24 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i2543 Int)
(declare-var i2542 Int)
(declare-var b4 Bool)
(declare-var i1 Int)
(declare-var i0 Int)
(constraint
 (or
  (not
   (and (and (or (not (not (> i0 2))) (> i2543 i0)) (or (not (not (> i0 2))) b4)) (not (> i0 2))))
  (= (+ (+ i2542 i2543) i0) (xi_2 i1))))
(check-synth)
