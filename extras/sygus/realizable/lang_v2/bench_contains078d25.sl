(set-logic DTNIA)
(synth-fun xi_1 ((x34 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x34 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var x Int)
(declare-var i Int)
(declare-var i471 Int)
(declare-var i472 Int)
(constraint
 (or
  (not
   (and
    (and (and (and (>= i471 0) (<= i471 1)) (and (>= i472 0) (<= i472 1)))
     (or (not (and (and (and (>= i471 0) (<= i471 1)) (and (>= i472 0) (<= i472 1))) (< x i)))
      (= i472 0)))
    (< x i)))
  (= (ite (= i x) 1 (ite (= i471 1) 1 (ite (= i472 1) 1 0))) (xi_1 i471))))
(check-synth)
