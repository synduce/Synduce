(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun joinb ((x38 Int) (x39 Int) (x40 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int (Ic x38 x39 x40 (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i4 Int)
(declare-var i742 Int)
(declare-var i0 Int)
(declare-var i Int)
(constraint
 (or
  (not
   (and
    (and (>= i742 0)
     (and (or (not (> i i0)) (and (> i i0) (ite (= i 0) (= i0 0) (> i0 i742))))
      (or (not (> i i0)) (and (> i i0) (ite (= i 0) (= i 0) (> i0 i742))))))
    (not (> i i0))))
  (= (max i (max i0 i742)) (joinb i i0 i742))))
(constraint
 (or
  (not
   (and
    (and (>= i742 0)
     (and (or (not (> i i0)) (and (> i i0) (ite (= i 0) (= i0 0) (> i0 i742))))
      (or (not (> i i0)) (and (> i i0) (ite (= i 0) (= i 0) (> i0 i742))))))
    (not (> i i0))))
  (= (max i4 (max i (max i0 i742))) (joinb i i0 i742))))
(check-synth)
