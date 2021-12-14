(set-logic LIA)
(synth-fun xi_2 ((x80 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x80 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i14211 Int)
(declare-var i14210 Int)
(declare-var b14 Bool)
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
          (and (or (not (not (> i0 2))) (= i1 (+ i14210 i14211)))
           (or (not (not (> i0 2))) (= i1 (+ i14210 i14211))))
          (or (not (not (> i0 2))) (= i1 (+ i14210 i14211))))
         (or (not (not (> i0 2))) (= i14210 1)))
        (or (not (not (> i0 2))) (= i0 2)))
       (or (not (not (> i0 2))) (> i14211 i0)))
      (or (not (not (> i0 2))) b14))
     (and (or (not (> i0 2)) (= i1 (+ i14210 i14211))) (or (not (> i0 2)) (> i14211 i0))))
    (not (> i0 2))))
  (= (+ (+ i14210 i14211) i0) (xi_2 i1))))
(check-synth)
