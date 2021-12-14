(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Bool
 ((mk_synd_tup_Int_Int_Bool (proj_synd_tup_Int_Int_Bool_0 Int) (proj_synd_tup_Int_Int_Bool_1 Int)
   (proj_synd_tup_Int_Int_Bool_2 Bool))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun odot$2 ((x23 synd_tup_Int_Int_Bool) (x24 synd_tup_Int_Int_Bool)) Bool
 ((IStart Bool) (Ipred Bool) (Ix Int) (Ic Int))
 ((IStart Bool ((> Ix Ix)))
  (Ipred Bool
   ((proj_synd_tup_Int_Int_Bool_2 x23) (proj_synd_tup_Int_Int_Bool_2 x24) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred) (= Ix Ix) (> Ix Ix) 
    (>= Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Bool_0 x23) (proj_synd_tup_Int_Int_Bool_1 x23)
    (proj_synd_tup_Int_Int_Bool_0 x24) (proj_synd_tup_Int_Int_Bool_1 x24) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i5 Int)
(declare-var i4 Int)
(declare-var b3 Bool)
(declare-var i9 Int)
(declare-var i8 Int)
(declare-var i Int)
(constraint
 (= (> i i9) (odot$2 (mk_synd_tup_Int_Int_Bool i i true) (mk_synd_tup_Int_Int_Bool i8 i9 b3))))
(constraint
 (= (> i4 (max i9 i5))
  (odot$2 (mk_synd_tup_Int_Int_Bool i4 (max i5 i4) (> i4 i5)) (mk_synd_tup_Int_Int_Bool i8 i9 b3))))
(check-synth)
