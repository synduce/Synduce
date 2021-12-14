(set-logic LIA)
(synth-fun f_a_lt_lo ((x45 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix))) (Ix Int (Ic x45 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i2174 Int)
(declare-var i2173 Int)
(declare-var i Int)
(declare-var lo Int)
(declare-var hi Int)
(constraint
 (or
  (not
   (and
    (and (and (>= i2173 0) (>= i2174 0))
     (and (and (or (not (< i lo)) (= i2173 0)) (or (not (< i lo)) (= i2173 0)))
      (or (not (and (not (< i lo)) (> i hi))) (= i2174 0))))
    (< i lo)))
  (= (ite (and (> hi i) (> i lo)) (+ (+ i i2173) i2174) (+ i2173 i2174)) (f_a_lt_lo i2174))))
(check-synth)
