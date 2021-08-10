(set-logic DTNIA)
(synth-fun s0 () Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var p1 Int)
(constraint (or (not (and (> p1 0) (= (mod p1 2) 0))) (= (ite (= (mod p1 2) 1) p1 0) s0)))
(check-synth)
