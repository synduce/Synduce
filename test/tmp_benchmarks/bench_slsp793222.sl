(set-logic DTLIA)
(declare-datatype synd_tup_Int_Bool
 ((mk_synd_tup_Int_Bool (proj_synd_tup_Int_Bool_0 Int) (proj_synd_tup_Int_Bool_1 Bool))))
(synth-fun f1$0 ((x15 synd_tup_Int_Bool)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix)))
  (Ix Int (Ic (proj_synd_tup_Int_Bool_0 x15) (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool
   ((proj_synd_tup_Int_Bool_1 x15) (= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred)
    (or Ipred Ipred)))))
(declare-var b2 Bool)
(declare-var i655 Int)
(declare-var i2 Int)
(declare-var i Int)
(constraint
 (or
  (not
   (and
    (and (>= i655 0)
     (and (or (not (<= i 0)) (> i (ite (= i i655) i i2))) (or (not (<= i 0)) (> i655 i))))
    (<= i 0)))
  (= (ite (and (>= i2 0) b2) (+ i655 i2) i655) (f1$0 (mk_synd_tup_Int_Bool i655 b2)))))
(check-synth)
