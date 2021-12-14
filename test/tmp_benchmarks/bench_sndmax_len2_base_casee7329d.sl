(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun odot$0 ((x28 Int) (x29 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix))) (Ix Int (Ic x28 x29 (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i134 Int)
(declare-var i133 Int)
(declare-var i0 Int)
(declare-var i Int)
(constraint
 (or
  (not
   (and (>= i133 i134)
    (and
     (and (and (and (> i i0) (and (> i0 i133) (> i 0))) (and (> i i0) (and (> i i133) (> i0 0))))
      (and (> i i0) (and (= i i) (> i0 0))))
     (and (> i i0) (and (= i i) (> i 0))))))
  (= (max i (max i0 i133)) (odot$0 i i0))))
(check-synth)
