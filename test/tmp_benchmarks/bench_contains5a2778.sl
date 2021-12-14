(set-logic LIA)
(synth-fun xi_2 ((x20 Int) (x21 Int) (x22 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix))) (Ix Int (Ic x20 x21 x22 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i575 Int)
(declare-var i574 Int)
(declare-var p Int)
(declare-var x Int)
(constraint
 (or (not (and (and (and (>= i574 0) (<= i574 1)) (and (>= i575 0) (<= i575 1))) (= i575 0)))
  (= (ite (= p x) 1 (ite (= i574 1) 1 (ite (= i575 1) 1 0))) (xi_2 x p i574))))
(check-synth)
