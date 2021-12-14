(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun joina ((x55 Int) (x56 Int) (x57 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int (Ic x55 x56 x57 (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i4 Int)
(declare-var i1076 Int)
(declare-var i0 Int)
(declare-var i Int)
(constraint
 (or
  (not
   (and
    (and (>= i1076 0)
     (and (or (not (not (> i i0))) (and (> i 0) (ite (= 0 i) (= 0 i) (= i i))))
      (and
       (and (or (not (> i i0)) (and (> i i0) (ite (= i 0) (= i0 0) (> i0 i1076))))
        (or (not (> i i0)) (and (> i i0) (ite (= i 0) (= i0 0) (> i0 i1076)))))
       (or (not (> i i0)) (and (> i i0) (ite (= i 0) (= i 0) (> i0 i1076)))))))
    (> i i0)))
  (= (max i (max i0 i1076)) (joina i i0 0))))
(constraint
 (or
  (not
   (and
    (and (>= i1076 0)
     (and (or (not (not (> i i0))) (and (> i i4) (ite (= 0 i) (= 0 i) (= i i))))
      (and
       (and (or (not (> i i0)) (and (> i i0) (ite (= i 0) (= i0 0) (> i0 i1076))))
        (or (not (> i i0)) (and (> i i0) (ite (= i 0) (= i0 0) (> i0 i1076)))))
       (or (not (> i i0)) (and (> i i0) (ite (= i 0) (= i 0) (> i0 i1076)))))))
    (> i i0)))
  (= (max i4 (max i (max i0 i1076))) (joina i i0 (max i4 0)))))
(check-synth)
