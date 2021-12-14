(set-logic DTLIA)
(declare-datatype synd_tup_Int_Bool_Int
 ((mk_synd_tup_Int_Bool_Int (proj_synd_tup_Int_Bool_Int_0 Int) (proj_synd_tup_Int_Bool_Int_1 Bool)
   (proj_synd_tup_Int_Bool_Int_2 Int))))
(synth-fun odot$1 ((x96 synd_tup_Int_Bool_Int) (x97 synd_tup_Int_Bool_Int)) Bool
 ((IStart Bool) (Ipred Bool) (Ix Int) (Ic Int))
 ((IStart Bool ((and Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Int_Bool_Int_1 x96) (proj_synd_tup_Int_Bool_Int_1 x97) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred) (= Ix Ix) (> Ix Ix) 
    (>= Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Bool_Int_0 x96) (proj_synd_tup_Int_Bool_Int_2 x96)
    (proj_synd_tup_Int_Bool_Int_0 x97) (proj_synd_tup_Int_Bool_Int_2 x97) 
    (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i13 Int)
(declare-var i12 Int)
(declare-var i3 Int)
(declare-var i2 Int)
(declare-var _elim_i8 Int)
(declare-var _elim_b3 Bool)
(declare-var _elim_i7 Int)
(declare-var i Int)
(constraint
 (= (and _elim_b3 (< i _elim_i7))
  (odot$1 (mk_synd_tup_Int_Bool_Int i true (+ (- 0) i))
   (mk_synd_tup_Int_Bool_Int _elim_i7 _elim_b3 _elim_i8))))
(constraint
 (= (and (and _elim_b3 (< i3 _elim_i7)) (< i2 i3))
  (odot$1 (mk_synd_tup_Int_Bool_Int i2 (and true (< i2 i3)) i3)
   (mk_synd_tup_Int_Bool_Int _elim_i7 _elim_b3 _elim_i8))))
(constraint
 (= (and (and (and _elim_b3 (< i13 _elim_i7)) (< i12 i13)) (< i2 i12))
  (odot$1 (mk_synd_tup_Int_Bool_Int i2 (and (and true (< i12 i13)) (< i2 i12)) i13)
   (mk_synd_tup_Int_Bool_Int _elim_i7 _elim_b3 _elim_i8))))
(check-synth)
