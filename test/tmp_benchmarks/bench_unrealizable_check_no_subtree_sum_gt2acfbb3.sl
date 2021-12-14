(set-logic LIA)
(synth-fun xi_1 ((x57 Int) (x58 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x57 x58 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i7550 Int)
(declare-var i7549 Int)
(declare-var b10 Bool)
(declare-var i1 Int)
(declare-var i0 Int)
(constraint
 (or
  (not
   (and
    (and
     (and
      (and
       (and (or (not (not (> i0 2))) (= i1 (+ i7549 i7550))) (or (not (not (> i0 2))) (= i7549 1)))
       (or (not (not (> i0 2))) (= i0 2)))
      (or (not (not (> i0 2))) (> i7550 i0)))
     (or (not (not (> i0 2))) b10))
    (> i0 2)))
  (= (+ (+ i7549 i7550) i0) (xi_1 i1 i0))))
(check-synth)
