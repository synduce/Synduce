(set-logic DTNIA)
(synth-fun f0 () Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var p Int)
(declare-var i54 Int)
(constraint
 (or (not (and (or (not (= (mod p 2) 0)) (= i54 (mod p 2))) (= (mod p 2) 0)))
  (= (ite (= (mod p 2) 1) p i54) f0)))
(check-synth)
