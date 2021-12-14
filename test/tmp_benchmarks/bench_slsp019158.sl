(set-logic DTLIA)
(declare-datatype synd_tup_Int_Bool
 ((mk_synd_tup_Int_Bool (proj_synd_tup_Int_Bool_0 Int) (proj_synd_tup_Int_Bool_1 Bool))))
(synth-fun f1$1 ((x2 synd_tup_Int_Bool)) Bool ((IStart Bool) (Ipred Bool) (Ix Int) (Ic Int))
 ((IStart Bool ((and Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Int_Bool_1 x2) (not Ipred) (and Ipred Ipred) (or Ipred Ipred) 
    (= Ix Ix) (> Ix Ix) (>= Ix Ix)))
  (Ix Int (Ic (proj_synd_tup_Int_Bool_0 x2) (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var b0 Bool)
(declare-var i3 Int)
(declare-var i2 Int)
(declare-var i Int)
(constraint
 (or (not (and (>= i3 0) (<= i 0))) (= (and (>= i2 0) b0) (f1$1 (mk_synd_tup_Int_Bool i3 b0)))))
(check-synth)
