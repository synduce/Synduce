(set-logic DTLIA)
(declare-datatype synd_tup_Bool_Int_Int
 ((mk_synd_tup_Bool_Int_Int (proj_synd_tup_Bool_Int_Int_0 Bool) (proj_synd_tup_Bool_Int_Int_1 Int)
   (proj_synd_tup_Bool_Int_Int_2 Int))))
(synth-fun f1$0 ((x48 Int) (x49 Int) (x50 synd_tup_Bool_Int_Int)) Bool
 ((Ipred Bool) (Ix Int) (Ic Int))
 ((Ipred Bool
   ((proj_synd_tup_Bool_Int_Int_0 x50) (not Ipred) (and Ipred Ipred) 
    (or Ipred Ipred) (= Ix Ix) (> Ix Ix) (>= Ix Ix)))
  (Ix Int
   (Ic x48 x49 (proj_synd_tup_Bool_Int_Int_1 x50) (proj_synd_tup_Bool_Int_Int_2 x50) 
    (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i8 Int)
(declare-var i7 Int)
(declare-var i6 Int)
(declare-var i5 Int)
(declare-var i4 Int)
(declare-var i3 Int)
(declare-var i2 Int)
(declare-var i1 Int)
(constraint
 (or (not (and (< i1 i2) (and (< i1 i3) true)))
  (= (and (not (< i4 i1)) (not (< i2 i3))) (f1$0 i1 i2 (mk_synd_tup_Bool_Int_Int false i3 i4)))))
(constraint
 (or (not (and (< i1 i2) (and (< i1 i5) (and (< i5 i6) (and (< i5 i7) true)))))
  (=
   (or (or (and (not (< i6 i1)) (not (< i2 i5))) (and (not (< i8 i1)) (not (< i2 i7))))
    (and (not (< i8 i5)) (not (< i6 i7))))
   (f1$0 i1 i2 (mk_synd_tup_Bool_Int_Int (or (and (not (< i8 i5)) (not (< i6 i7))) false) i5 i6)))))
(check-synth)
