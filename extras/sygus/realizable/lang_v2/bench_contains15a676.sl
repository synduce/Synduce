(set-logic DTNIA)
(synth-fun xi_1 ((x96 Int) (x97 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x96 x97 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var x Int)
(declare-var p Int)
(declare-var i35 Int)
(constraint
 (or
  (not
   (and (and (>= i35 0) (<= i35 1))
    (or (not (and (>= i35 0) (<= i35 1))) (= p (ite (= i35 0) p x)))))
  (= (ite (= p x) 1 i35) (xi_1 x p))))
(check-synth)
