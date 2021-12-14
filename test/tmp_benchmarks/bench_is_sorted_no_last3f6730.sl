(set-logic DTLIA)
(declare-datatype synd_tup_Int_Bool_Int
 ((mk_synd_tup_Int_Bool_Int (proj_synd_tup_Int_Bool_Int_0 Int) (proj_synd_tup_Int_Bool_Int_1 Bool)
   (proj_synd_tup_Int_Bool_Int_2 Int))))
(synth-fun odot$0 ((x37 synd_tup_Int_Bool_Int) (x38 synd_tup_Int_Bool_Int)) Int
 ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic (proj_synd_tup_Int_Bool_Int_0 x37) (proj_synd_tup_Int_Bool_Int_2 x37)
    (proj_synd_tup_Int_Bool_Int_0 x38) (proj_synd_tup_Int_Bool_Int_2 x38) 
    (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool
   ((proj_synd_tup_Int_Bool_Int_1 x37) (proj_synd_tup_Int_Bool_Int_1 x38) 
    (= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun odot$1 ((x39 synd_tup_Int_Bool_Int) (x40 synd_tup_Int_Bool_Int)) Bool
 ((IStart Bool) (Ipred Bool) (Ix Int) (Ic Int))
 ((IStart Bool ((and Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Int_Bool_Int_1 x39) (proj_synd_tup_Int_Bool_Int_1 x40) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred) (= Ix Ix) (> Ix Ix) 
    (>= Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Bool_Int_0 x39) (proj_synd_tup_Int_Bool_Int_2 x39)
    (proj_synd_tup_Int_Bool_Int_0 x40) (proj_synd_tup_Int_Bool_Int_2 x40) 
    (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))))
(synth-fun odot$2 ((x41 synd_tup_Int_Bool_Int) (x42 synd_tup_Int_Bool_Int)) Int
 ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic (proj_synd_tup_Int_Bool_Int_0 x41) (proj_synd_tup_Int_Bool_Int_2 x41)
    (proj_synd_tup_Int_Bool_Int_0 x42) (proj_synd_tup_Int_Bool_Int_2 x42) 
    (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool
   ((proj_synd_tup_Int_Bool_Int_1 x41) (proj_synd_tup_Int_Bool_Int_1 x42) 
    (= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun f0$2 ((x43 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x43 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i3 Int)
(declare-var i2 Int)
(declare-var _elim_i0 Int)
(declare-var _elim_b Bool)
(declare-var _elim_i Int)
(declare-var i Int)
(constraint
 (= i
  (odot$0 (mk_synd_tup_Int_Bool_Int i true (f0$2 i))
   (mk_synd_tup_Int_Bool_Int _elim_i _elim_b _elim_i0))))
(constraint
 (= (and _elim_b (< i _elim_i))
  (odot$1 (mk_synd_tup_Int_Bool_Int i true (f0$2 i))
   (mk_synd_tup_Int_Bool_Int _elim_i _elim_b _elim_i0))))
(constraint
 (= i2
  (odot$0 (mk_synd_tup_Int_Bool_Int i2 (and true (< i2 i3)) i3)
   (mk_synd_tup_Int_Bool_Int _elim_i _elim_b _elim_i0))))
(constraint
 (= (and (and _elim_b (< i3 _elim_i)) (< i2 i3))
  (odot$1 (mk_synd_tup_Int_Bool_Int i2 (and true (< i2 i3)) i3)
   (mk_synd_tup_Int_Bool_Int _elim_i _elim_b _elim_i0))))
(constraint
 (= i3
  (odot$2 (mk_synd_tup_Int_Bool_Int i2 true (f0$2 i2))
   (mk_synd_tup_Int_Bool_Int i3 true (f0$2 i3)))))
(check-synth)
