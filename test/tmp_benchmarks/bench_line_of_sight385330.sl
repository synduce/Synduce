(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Bool
 ((mk_synd_tup_Int_Int_Bool (proj_synd_tup_Int_Int_Bool_0 Int) (proj_synd_tup_Int_Int_Bool_1 Int)
   (proj_synd_tup_Int_Int_Bool_2 Bool))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun odot$0 ((x31 synd_tup_Int_Int_Bool) (x32 synd_tup_Int_Int_Bool)) Int
 ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic (proj_synd_tup_Int_Int_Bool_0 x31) (proj_synd_tup_Int_Int_Bool_1 x31)
    (proj_synd_tup_Int_Int_Bool_0 x32) (proj_synd_tup_Int_Int_Bool_1 x32) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool
   ((proj_synd_tup_Int_Int_Bool_2 x31) (proj_synd_tup_Int_Int_Bool_2 x32) 
    (= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i5 Int)
(declare-var i4 Int)
(declare-var b3 Bool)
(declare-var i9 Int)
(declare-var i8 Int)
(declare-var i Int)
(constraint (= i (odot$0 (mk_synd_tup_Int_Int_Bool i i true) (mk_synd_tup_Int_Int_Bool i8 i9 b3))))
(constraint
 (= i4
  (odot$0 (mk_synd_tup_Int_Int_Bool i4 (max i5 i4) (> i4 i5)) (mk_synd_tup_Int_Int_Bool i8 i9 b3))))
(check-synth)
