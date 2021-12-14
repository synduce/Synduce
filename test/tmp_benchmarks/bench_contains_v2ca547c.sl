(set-logic LIA)
(synth-fun xi_1 ((x29 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix))) (Ix Int (Ic x29 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i1002 Int)
(declare-var i1001 Int)
(declare-var i Int)
(declare-var x Int)
(constraint
 (or
  (not
   (and
    (and
     (and (and (and (>= i1001 0) (<= i1001 1)) (and (>= i1002 0) (<= i1002 1)))
      (and (or (not (and (not (= x i)) (not (< x i)))) (= i1001 0))
       (and (or (not (and (not (= x i)) (< x i))) (= i1002 0))
        (or (not (and (not (= x i)) (< x i))) (= i1002 0)))))
     (not (= x i)))
    (< x i)))
  (= (ite (= i x) 1 (ite (= i1001 1) 1 (ite (= i1002 1) 1 0))) (xi_1 i1001))))
(check-synth)
