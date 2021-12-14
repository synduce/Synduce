(set-logic LIA)
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun f0 ((x3 Int) (x4 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((min Ix Ix))) (Ix Int (Ic x3 x4 (- Ix) (+ Ix Ix) (min Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i2 Int)
(declare-var i1 Int)
(declare-var i Int)
(constraint (= (min i (min i1 i2)) (f0 i i1)))
(check-synth)
