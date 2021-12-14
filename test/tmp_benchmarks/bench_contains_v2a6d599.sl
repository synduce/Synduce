(set-logic LIA)
(synth-fun xi_1 ((x13 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix))) (Ix Int (Ic x13 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i346 Int)
(declare-var i345 Int)
(declare-var i Int)
(declare-var x Int)
(constraint
 (or
  (not
   (and
    (and
     (and (and (and (>= i345 0) (<= i345 1)) (and (>= i346 0) (<= i346 1)))
      (or (not (and (not (= x i)) (< x i))) (= i346 0)))
     (not (= x i)))
    (< x i)))
  (= (ite (= i x) 1 (ite (= i345 1) 1 (ite (= i346 1) 1 0))) (xi_1 i345))))
(check-synth)
