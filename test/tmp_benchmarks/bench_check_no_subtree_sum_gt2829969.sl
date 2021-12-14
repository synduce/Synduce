(set-logic LIA)
(synth-fun xi_2 ((x66 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x66 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i10984 Int)
(declare-var i10983 Int)
(declare-var b12 Bool)
(declare-var i1 Int)
(declare-var i0 Int)
(constraint
 (or
  (not
   (and
    (and
     (and
      (and
       (and
        (and
         (and (or (not (not (> i0 2))) (= i1 (+ i10983 i10984)))
          (or (not (not (> i0 2))) (= i1 (+ i10983 i10984))))
         (or (not (not (> i0 2))) (= i10983 1)))
        (or (not (not (> i0 2))) (= i0 2)))
       (or (not (not (> i0 2))) (> i10984 i0)))
      (or (not (not (> i0 2))) b12))
     (or (not (> i0 2)) (> i10984 i0)))
    (not (> i0 2))))
  (= (+ (+ i10983 i10984) i0) (xi_2 i1))))
(check-synth)
