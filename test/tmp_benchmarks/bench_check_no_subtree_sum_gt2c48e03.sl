(set-logic LIA)
(synth-fun xi_2 ((x91 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x91 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i17111 Int)
(declare-var i17110 Int)
(declare-var b16 Bool)
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
         (and
          (and
           (and (or (not (not (> i0 2))) (= i1 (+ i17110 i17111)))
            (or (not (not (> i0 2))) (= i1 (+ i17110 i17111))))
           (or (not (not (> i0 2))) (= i1 (+ i17110 i17111))))
          (or (not (not (> i0 2))) (= i1 (+ i17110 i17111))))
         (or (not (not (> i0 2))) (= i17110 1)))
        (or (not (not (> i0 2))) (= i0 2)))
       (or (not (not (> i0 2))) (> i17111 i0)))
      (or (not (not (> i0 2))) b16))
     (and (and (or (not (> i0 2)) (> i17110 0)) (or (not (> i0 2)) (= i1 (+ i17110 i17111))))
      (or (not (> i0 2)) (> i17111 i0))))
    (not (> i0 2))))
  (= (+ (+ i17110 i17111) i0) (xi_2 i1))))
(check-synth)
