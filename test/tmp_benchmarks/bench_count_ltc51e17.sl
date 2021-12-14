(set-logic LIA)
(synth-fun xi_2 ((x14 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x14 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i980 Int)
(declare-var i979 Int)
(declare-var i Int)
(declare-var x Int)
(constraint
 (or
  (not (and (and (and (>= i979 0) (>= i980 0)) (or (not (not (< i x))) (= i980 0))) (not (< i x))))
  (= (+ i979 i980) (xi_2 i979))))
(check-synth)
