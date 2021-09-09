(set-logic DTNIA)
(synth-fun xi_1 ((x63 Int) (x64 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x63 x64 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var x Int)
(declare-var i Int)
(declare-var i27 Int)
(constraint
 (or
  (not
   (and (and (>= i27 0) (or (not (and (>= i27 0) (>= i x))) (= i (ite (= i27 0) i x)))) (>= i x)))
  (= (ite (= i x) 1 (ite (= i27 0) 0 (+ 1 i27))) (xi_1 x i))))
(check-synth)
