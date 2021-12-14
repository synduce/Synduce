(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join2 ((x17 Int) (x18 Int) (x19 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x17 x18 x19 (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i2 Int)
(declare-var i0 Int)
(declare-var p7 Int)
(declare-var p Int)
(constraint (= (max p i2) (max p (join2 p7 i0 i2))))
(check-synth)
