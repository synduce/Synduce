(set-logic LIA)
(synth-fun f1 ((x3 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x3 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
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
    (< i0 2)))
  (= (+ (+ 1 i1237) i1238) (f1 i))))
(check-synth)
