(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(declare-datatype synd_tup_Int_Int_Bool
 ((mk_synd_tup_Int_Int_Bool (proj_synd_tup_Int_Int_Bool_0 Int) (proj_synd_tup_Int_Int_Bool_1 Int)
   (proj_synd_tup_Int_Int_Bool_2 Bool))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun s1$2 ((x50 synd_tup_Int_Int) (x51 synd_tup_Int_Int_Bool)) Bool
 ((IStart Bool) (Ipred Bool) (Ix Int) (Ic Int))
 ((IStart Bool ((and Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Int_Int_Bool_2 x51) (not Ipred) (and Ipred Ipred) 
    (or Ipred Ipred) (= Ix Ix) (> Ix Ix) (>= Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x50) (proj_synd_tup_Int_Int_1 x50)
    (proj_synd_tup_Int_Int_Bool_0 x51) (proj_synd_tup_Int_Int_Bool_1 x51) 
    (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i14 Int)
(declare-var i1 Int)
(declare-var b5 Bool)
(declare-var i18 Int)
(declare-var i17 Int)
(declare-var i2 Int)
(constraint
 (= (and b5 (and (<= i17 i2) (>= i2 i18)))
  (s1$2 (mk_synd_tup_Int_Int i2 i2) (mk_synd_tup_Int_Int_Bool i17 i18 b5))))
(constraint
 (= (and b5 (and (<= i17 (min i1 i14)) (>= (max i1 i14) i18)))
  (s1$2 (mk_synd_tup_Int_Int (min i1 i14) (max i1 i14)) (mk_synd_tup_Int_Int_Bool i17 i18 b5))))
(check-synth)
