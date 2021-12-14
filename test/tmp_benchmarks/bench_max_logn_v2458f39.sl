(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun joinb ((x19 Int) (x20 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix))) (Ix Int (Ic x19 x20 (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i4894 Int)
(declare-var i4 Int)
(declare-var i0 Int)
(declare-var i Int)
(constraint
 (or
  (not
   (and (and (or (not (not (> i i0))) (> i i4)) (or (not (> i i0)) (> i0 i4894))) (not (> i i0))))
  (= (max i4 (max i (max i0 i4894))) (joinb i0 i4894))))
(check-synth)
