(set-logic LIA)
(synth-fun xi_2 ((x42 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x42 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i3714 Int)
(declare-var i3713 Int)
(declare-var b8 Bool)
(declare-var i1 Int)
(declare-var i0 Int)
(constraint
 (or
  (not
   (and
    (and
     (and (and (or (not (not (> i0 2))) (= i3713 1)) (or (not (not (> i0 2))) (= i0 2)))
      (or (not (not (> i0 2))) (> i3714 i0)))
     (or (not (not (> i0 2))) b8))
    (not (> i0 2))))
  (= (+ (+ i3713 i3714) i0) (xi_2 i1))))
(check-synth)
