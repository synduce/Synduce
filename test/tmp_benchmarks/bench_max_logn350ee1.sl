(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun joina ((x20 Int) (x21 Int) (x22 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int (Ic x20 x21 x22 (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i4 Int)
(declare-var i396 Int)
(declare-var i0 Int)
(declare-var i Int)
(constraint
 (or
  (not
   (and (and (>= i396 0) (or (not (> i i0)) (and (> i i0) (ite (= i 0) (= i 0) (> i0 i396)))))
    (> i i0)))
  (= (max i (max i0 i396)) (joina i i0 0))))
(constraint
 (or
  (not
   (and (and (>= i396 0) (or (not (> i i0)) (and (> i i0) (ite (= i 0) (= i 0) (> i0 i396)))))
    (> i i0)))
  (= (max i4 (max i (max i0 i396))) (joina i i0 (max i4 0)))))
(check-synth)
