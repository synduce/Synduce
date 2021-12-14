(set-logic LIA)
(synth-fun f_a_lt_lo ((x44 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix))) (Ix Int (Ic x44 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i2180 Int)
(declare-var i2179 Int)
(declare-var i Int)
(declare-var lo Int)
(declare-var hi Int)
(constraint
 (or
  (not
   (and
    (and (and (>= i2179 0) (>= i2180 0))
     (and (and (or (not (<= i lo)) (= i2179 0)) (or (not (<= i lo)) (= i2179 0)))
      (or (not (and (not (<= i lo)) (>= i hi))) (= i2180 0))))
    (<= i lo)))
  (= (ite (and (> hi i) (> i lo)) (+ (+ 1 i2179) i2180) (+ i2179 i2180)) (f_a_lt_lo i2180))))
(check-synth)
