(set-logic DTNIA)
(synth-fun xi_2 ((x45 Int) (x46 Int) (x47 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x45 x46 x47 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var x Int)
(declare-var p Int)
(declare-var i426 Int)
(declare-var i427 Int)
(constraint
 (or
  (not
   (and (and (and (>= i426 0) (<= i426 1)) (and (>= i427 0) (<= i427 1)))
    (or (not (and (and (>= i426 0) (<= i426 1)) (and (>= i427 0) (<= i427 1)))) (= i427 0))))
  (= (ite (= p x) 1 (ite (= i426 1) 1 (ite (= i427 1) 1 0))) (xi_2 x p i426))))
(check-synth)
