(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun joinb ((x5 Int) (x6 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix))) (Ix Int (Ic x5 x6 (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i5 Int)
(declare-var i4 Int)
(declare-var i0 Int)
(declare-var i Int)
(constraint (or (not (not (> i i0))) (= (max i4 (max i (max i0 i5))) (joinb i0 i5))))
(check-synth)
