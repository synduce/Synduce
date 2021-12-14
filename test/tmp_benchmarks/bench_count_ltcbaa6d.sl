(set-logic LIA)
(synth-fun f1 ((x9 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x9 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i2296 Int)
(declare-var i2295 Int)
(declare-var i0 Int)
(declare-var i Int)
(constraint
 (or
  (not
   (and
    (and (and (>= i2295 0) (>= i2296 0))
     (and (or (not (not (< i0 2))) (= i (+ i2295 i2296)))
      (and (or (not (< i0 2)) (= i (+ (+ i2295 i2296) 1)))
       (or (not (< i0 2)) (= i (+ (+ i2295 i2296) 1))))))
    (< i0 2)))
  (= (+ (+ 1 i2295) i2296) (f1 i))))
(check-synth)
