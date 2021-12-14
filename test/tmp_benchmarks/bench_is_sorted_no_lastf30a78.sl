(set-logic DTLIA)
(declare-datatype synd_tup_Int_Bool
 ((mk_synd_tup_Int_Bool (proj_synd_tup_Int_Bool_0 Int) (proj_synd_tup_Int_Bool_1 Bool))))
(synth-fun odot$1 ((x16 synd_tup_Int_Bool) (x17 synd_tup_Int_Bool)) Bool
 ((IStart Bool) (Ipred Bool) (Ix Int) (Ic Int))
 ((IStart Bool ((and Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Int_Bool_1 x16) (proj_synd_tup_Int_Bool_1 x17) (not Ipred) 
    (and Ipred Ipred) (or Ipred Ipred) (= Ix Ix) (> Ix Ix) (>= Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Bool_0 x16) (proj_synd_tup_Int_Bool_0 x17) 
    (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i3 Int)
(declare-var i2 Int)
(declare-var b3 Bool)
(declare-var i5 Int)
(declare-var i Int)
(constraint
 (= (and b3 (< i i5)) (odot$1 (mk_synd_tup_Int_Bool i true) (mk_synd_tup_Int_Bool i5 b3))))
(constraint
 (= (and (and b3 (< i3 i5)) (< i2 i3))
  (odot$1 (mk_synd_tup_Int_Bool i2 (and true (< i2 i3))) (mk_synd_tup_Int_Bool i5 b3))))
(check-synth)
