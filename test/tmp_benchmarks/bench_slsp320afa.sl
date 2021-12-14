(set-logic DTLIA)
(declare-datatype synd_tup_Int_Bool
 ((mk_synd_tup_Int_Bool (proj_synd_tup_Int_Bool_0 Int) (proj_synd_tup_Int_Bool_1 Bool))))
(synth-fun f1$1 ((x8 synd_tup_Int_Bool)) Bool ((IStart Bool) (Ipred Bool) (Ix Int) (Ic Int))
 ((IStart Bool ((and Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Int_Bool_1 x8) (not Ipred) (and Ipred Ipred) (or Ipred Ipred) 
    (= Ix Ix) (> Ix Ix) (>= Ix Ix)))
  (Ix Int (Ic (proj_synd_tup_Int_Bool_0 x8) (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var b1 Bool)
(declare-var i342 Int)
(declare-var i2 Int)
(declare-var i Int)
(constraint
 (or (not (and (and (>= i342 0) (or (not (<= i 0)) (> i342 i))) (<= i 0)))
  (= (and (>= i2 0) b1) (f1$1 (mk_synd_tup_Int_Bool i342 b1)))))
(check-synth)
