(set-logic LIA)
(synth-fun xi_1 ((x7 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x7 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i8650 Int)
(declare-var i8649 Int)
(declare-var i Int)
(constraint
 (or
  (not
   (and (and (>= i8649 1) (>= i8650 1))
    (and (= i (+ (+ i8649 i8650) 1)) (>= (+ i8649 (+ i8650 i8650)) i))))
  (= (+ (+ 1 i8649) i8650) (xi_1 i))))
(check-synth)
