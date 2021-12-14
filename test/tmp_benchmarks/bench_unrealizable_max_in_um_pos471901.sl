(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join ((x22 Int) (x23 Int) (x24 Int) (x25 Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int (Ic x22 x23 x24 x25 (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i4 Int)
(declare-var i1353 Int)
(declare-var i0 Int)
(declare-var i Int)
(constraint
 (or (not (and (>= i i) (ite (> i i0) (> i i1353) (= i i))))
  (= (max i (max i0 i1353)) (join i i0 0 i1353))))
(constraint
 (or (not (and (>= i i) (ite (>= i i0) (> i i1353) (= i i))))
  (= (max i4 (max i (max i0 i1353))) (join i i0 (max i4 0) i1353))))
(check-synth)
