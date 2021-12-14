(set-logic DTLIA)
(declare-datatype synd_tup_Int_Bool
 ((mk_synd_tup_Int_Bool (proj_synd_tup_Int_Bool_0 Int) (proj_synd_tup_Int_Bool_1 Bool))))
(synth-fun f1$1 ((x3 synd_tup_Int_Bool)) Bool ((IStart Bool) (Ipred Bool) (Ix Int) (Ic Int))
 ((IStart Bool ((and Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Int_Bool_1 x3) (not Ipred) (and Ipred Ipred) (or Ipred Ipred) 
    (= Ix Ix) (> Ix Ix) (>= Ix Ix)))
  (Ix Int (Ic (proj_synd_tup_Int_Bool_0 x3) (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var b0 Bool)
(declare-var i5 Int)
(declare-var i4 Int)
(declare-var i Int)
(constraint
 (or (not (and (and (>= i5 0) (= b0 (= i5 0))) (<= i 0)))
  (= (and b0 (>= i4 0)) (f1$1 (mk_synd_tup_Int_Bool i5 b0)))))
(check-synth)
