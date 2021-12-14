(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun odot$1 ((x16 Int) (x17 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int (Ic x16 x17 (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i34 Int)
(declare-var i33 Int)
(declare-var i0 Int)
(declare-var i Int)
(constraint
 (or (not (and (>= i33 i34) (and (and (and (> i 0) (> i i0)) (> i0 0)) (= i i))))
  (= (max (max i34 (min i0 i33)) (min i (max i0 i33))) (odot$1 i i0))))
(check-synth)
