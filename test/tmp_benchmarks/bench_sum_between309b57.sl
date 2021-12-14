(set-logic LIA)
(synth-fun f_a_lt_lo ((x9 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix))) (Ix Int (Ic x9 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i2 Int)
(declare-var i1 Int)
(declare-var i Int)
(declare-var lo Int)
(declare-var hi Int)
(constraint
 (or (not (and (and (>= i1 0) (>= i2 0)) (< i lo)))
  (= (ite (and (> hi i) (> i lo)) (+ (+ i i1) i2) (+ i1 i2)) (f_a_lt_lo i2))))
(check-synth)
