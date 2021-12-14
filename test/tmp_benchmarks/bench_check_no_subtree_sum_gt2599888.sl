(set-logic LIA)
(synth-fun xi_2 ((x33 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x33 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i2866 Int)
(declare-var i2865 Int)
(declare-var b6 Bool)
(declare-var i1 Int)
(declare-var i0 Int)
(constraint
 (or
  (not
   (and
    (and (and (or (not (not (> i0 2))) (= i0 2)) (or (not (not (> i0 2))) (> i2866 i0)))
     (or (not (not (> i0 2))) b6))
    (not (> i0 2))))
  (= (+ (+ i2865 i2866) i0) (xi_2 i1))))
(check-synth)
