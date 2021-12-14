(set-logic DTLIA)
(declare-datatype synd_tup_Int_Bool
 ((mk_synd_tup_Int_Bool (proj_synd_tup_Int_Bool_0 Int) (proj_synd_tup_Int_Bool_1 Bool))))
(synth-fun f1$1 ((x7 synd_tup_Int_Bool)) Bool ((IStart Bool) (Ipred Bool) (Ix Int) (Ic Int))
 ((IStart Bool ((and Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Int_Bool_1 x7) (not Ipred) (and Ipred Ipred) (or Ipred Ipred) 
    (= Ix Ix) (> Ix Ix) (>= Ix Ix)))
  (Ix Int (Ic (proj_synd_tup_Int_Bool_0 x7) (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var b1 Bool)
(declare-var i528 Int)
(declare-var i4 Int)
(declare-var i Int)
(constraint
 (or (not (and (and (and (>= i528 0) (= b1 (= i528 0))) (or (not (<= i 0)) (> i528 i))) (<= i 0)))
  (= (and b1 (>= i4 0)) (f1$1 (mk_synd_tup_Int_Bool i528 b1)))))
(check-synth)
