(set-logic LIA)
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun join$0 ((x94 Int) (x95 Int) (x96 Int) (x97 Int) (x98 Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((min Ix Ix)))
  (Ix Int (Ic x94 x95 x96 x97 x98 (- Ix) (+ Ix Ix) (min Ix Ix) (ite Ipred Ix Ix)))
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
  (= (min i3 (min i8043 i8045)) (join$0 i3 i i0 i1 i2))))
(check-synth)
