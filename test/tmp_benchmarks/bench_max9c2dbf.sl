(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join ((x6 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix))) (Ix Int (Ic x6 (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i12 Int)
(declare-var p Int)
(constraint (or (not (>= p i12)) (= (max p i12) (join p))))
(check-synth)
