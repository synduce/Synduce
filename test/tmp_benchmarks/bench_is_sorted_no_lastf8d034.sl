(set-logic DTLIA)
(declare-datatype synd_tup_Int_Bool_Int
 ((mk_synd_tup_Int_Bool_Int (proj_synd_tup_Int_Bool_Int_0 Int) (proj_synd_tup_Int_Bool_Int_1 Bool)
   (proj_synd_tup_Int_Bool_Int_2 Int))))
(synth-fun odot$0 ((x102 synd_tup_Int_Bool_Int) (x103 synd_tup_Int_Bool_Int)) Int
 ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic (proj_synd_tup_Int_Bool_Int_0 x102) (proj_synd_tup_Int_Bool_Int_2 x102)
    (proj_synd_tup_Int_Bool_Int_0 x103) (proj_synd_tup_Int_Bool_Int_2 x103) 
    (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool
   ((proj_synd_tup_Int_Bool_Int_1 x102) (proj_synd_tup_Int_Bool_Int_1 x103) 
    (= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i13 Int)
(declare-var i12 Int)
(declare-var i3 Int)
(declare-var i2 Int)
(declare-var _elim_i8 Int)
(declare-var _elim_b3 Bool)
(declare-var _elim_i7 Int)
(declare-var i Int)
(constraint
 (= i
  (odot$0 (mk_synd_tup_Int_Bool_Int i true (+ (- 0) i))
   (mk_synd_tup_Int_Bool_Int _elim_i7 _elim_b3 _elim_i8))))
(constraint
 (= i2
  (odot$0 (mk_synd_tup_Int_Bool_Int i2 (and true (< i2 i3)) i3)
   (mk_synd_tup_Int_Bool_Int _elim_i7 _elim_b3 _elim_i8))))
(constraint
 (= i2
  (odot$0 (mk_synd_tup_Int_Bool_Int i2 (and (and true (< i12 i13)) (< i2 i12)) i13)
   (mk_synd_tup_Int_Bool_Int _elim_i7 _elim_b3 _elim_i8))))
(check-synth)
