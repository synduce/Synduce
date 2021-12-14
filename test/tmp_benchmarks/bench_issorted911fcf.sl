(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Bool
 ((mk_synd_tup_Int_Int_Bool (proj_synd_tup_Int_Int_Bool_0 Int) (proj_synd_tup_Int_Int_Bool_1 Int)
   (proj_synd_tup_Int_Int_Bool_2 Bool))))
(synth-fun odot ((x4 synd_tup_Int_Int_Bool) (x5 synd_tup_Int_Int_Bool)) synd_tup_Int_Int_Bool
 ((Tr synd_tup_Int_Int_Bool) (Ix Int) (Ic Int) (Ipred Bool))
 ((Tr synd_tup_Int_Int_Bool ((mk_synd_tup_Int_Int_Bool Ix Ix Ipred)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Bool_0 x4) (proj_synd_tup_Int_Int_Bool_1 x4)
    (proj_synd_tup_Int_Int_Bool_0 x5) (proj_synd_tup_Int_Int_Bool_1 x5) 
    (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool
   ((proj_synd_tup_Int_Int_Bool_2 x4) (proj_synd_tup_Int_Int_Bool_2 x5) 
    (= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var p10 Int)
(declare-var p7 Int)
(declare-var b3 Bool)
(declare-var i6 Int)
(declare-var i5 Int)
(declare-var p4 Int)
(constraint
 (= (mk_synd_tup_Int_Int_Bool p4 i6 (and b3 (< p4 i5)))
  (odot (mk_synd_tup_Int_Int_Bool p4 p4 true) (mk_synd_tup_Int_Int_Bool i5 i6 b3))))
(constraint
 (= (mk_synd_tup_Int_Int_Bool p7 i6 (and (and b3 (< p10 i5)) (< p7 p10)))
  (odot (mk_synd_tup_Int_Int_Bool p7 p10 (and true (< p7 p10)))
   (mk_synd_tup_Int_Int_Bool i5 i6 b3))))
(check-synth)
