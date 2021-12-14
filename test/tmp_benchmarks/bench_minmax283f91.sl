(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join$1 ((x61 Int) (x62 Int) (x63 Int) (x64 Int) (x65 Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int (Ic x61 x62 x63 x64 x65 (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i6062 Int)
(declare-var i6061 Int)
(declare-var i6060 Int)
(declare-var i6059 Int)
(declare-var i3 Int)
(declare-var i2 Int)
(declare-var i1 Int)
(declare-var i0 Int)
(declare-var i Int)
(constraint
 (or
  (not
   (and (and (<= i6059 i6060) (<= i6061 i6062)) (and (and (= i1 i6061) (= i0 i6060)) (= i i6059))))
  (= (max i3 (max i6060 i6062)) (join$1 i3 i i0 i1 i2))))
(check-synth)
