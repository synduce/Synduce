(set-logic LIA)
(synth-fun f2 ((x6 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x6 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i1238 Int)
(declare-var i1237 Int)
(declare-var i0 Int)
(declare-var i Int)
(constraint
 (or
  (not
   (and (and (and (>= i1237 0) (>= i1238 0)) (or (not (< i0 2)) (= i (+ (+ i1237 i1238) 1))))
    (not (< i0 2))))
  (= (+ i1237 i1238) (f2 i))))
(check-synth)
