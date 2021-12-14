(set-logic DTLIA)
(declare-datatype synd_tup_Int_Bool
 ((mk_synd_tup_Int_Bool (proj_synd_tup_Int_Bool_0 Int) (proj_synd_tup_Int_Bool_1 Bool))))
(synth-fun f1$1 ((x13 synd_tup_Int_Bool)) Bool ((IStart Bool) (Ipred Bool) (Ix Int) (Ic Int))
 ((IStart Bool ((and Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Int_Bool_1 x13) (not Ipred) (and Ipred Ipred) (or Ipred Ipred) 
    (= Ix Ix) (> Ix Ix) (>= Ix Ix)))
  (Ix Int (Ic (proj_synd_tup_Int_Bool_0 x13) (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))))
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
  (= (and (>= i2 0) b2) (f1$1 (mk_synd_tup_Int_Bool i655 b2)))))
(check-synth)
