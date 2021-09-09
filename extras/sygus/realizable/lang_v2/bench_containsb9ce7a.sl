(set-logic DTNIA)
(synth-fun xi_2 ((x45 Int) (x46 Int) (x47 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x45 x46 x47 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var x Int)
(declare-var p Int)
(declare-var i666 Int)
(declare-var i667 Int)
(constraint
 (or
  (not
   (and (and (and (>= i666 0) (<= i666 1)) (and (>= i667 0) (<= i667 1)))
    (or (not (and (and (>= i666 0) (<= i666 1)) (and (>= i667 0) (<= i667 1)))) (= i667 0))))
  (= (ite (= p x) 1 (ite (= i666 1) 1 (ite (= i667 1) 1 0))) (xi_2 x p i666))))
(check-synth)
