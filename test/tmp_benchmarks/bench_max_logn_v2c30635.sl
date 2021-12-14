(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun joina ((x14 Int) (x15 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix))) (Ix Int (Ic x14 x15 (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i1161 Int)
(declare-var i4 Int)
(declare-var i0 Int)
(declare-var i Int)
(constraint
 (or (not (and (or (not (not (> i i0))) (> i i4)) (> i i0)))
  (= (max i4 (max i (max i0 i1161))) (joina i i4))))
(check-synth)
