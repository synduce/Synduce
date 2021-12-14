(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun odot$1 ((x24 Int) (x25 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int (Ic x24 x25 (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
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
  (= (max (max i134 (min i0 i133)) (min i (max i0 i133))) (odot$1 i i0))))
(check-synth)
