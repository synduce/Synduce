(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join$1 ((x84 Int) (x85 Int) (x86 Int) (x87 Int) (x88 Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int (Ic x84 x85 x86 x87 x88 (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i8046 Int)
(declare-var i8045 Int)
(declare-var i8044 Int)
(declare-var i8043 Int)
(declare-var i3 Int)
(declare-var i2 Int)
(declare-var i1 Int)
(declare-var i0 Int)
(declare-var i Int)
(constraint
 (or
  (not
   (and (and (<= i8043 i8044) (<= i8045 i8046))
    (and (and (and (= i2 i8046) (= i1 i8045)) (= i0 i8044)) (= i i8043))))
  (= (max i3 (max i8044 i8046)) (join$1 i3 i i0 i1 i2))))
(check-synth)
