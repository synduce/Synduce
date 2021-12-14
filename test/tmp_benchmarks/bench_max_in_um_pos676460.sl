(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join ((x31 Int) (x32 Int) (x33 Int) (x34 Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int (Ic x31 x32 x33 x34 (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i4 Int)
(declare-var i2482 Int)
(declare-var i0 Int)
(declare-var i Int)
(constraint
 (or
  (not
   (and (and (>= i i) (ite (> i i0) (> i0 i2482) (= i i)))
    (and (>= i i) (ite (> i i0) (> i i2482) (= i i)))))
  (= (max i (max i0 i2482)) (join i i0 0 i2482))))
(constraint
 (or
  (not
   (and (and (>= i i) (ite (>= i i0) (> i0 i2482) (> i i4)))
    (and (>= i i) (ite (>= i i0) (> i i2482) (= i i)))))
  (= (max i4 (max i (max i0 i2482))) (join i i0 (max i4 0) i2482))))
(check-synth)
