(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(declare-datatype synd_tup_Int_Int_Bool
 ((mk_synd_tup_Int_Int_Bool (proj_synd_tup_Int_Int_Bool_0 Int) (proj_synd_tup_Int_Int_Bool_1 Int)
   (proj_synd_tup_Int_Int_Bool_2 Bool))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun s1$0 ((x56 synd_tup_Int_Int) (x57 synd_tup_Int_Int_Bool)) Int
 ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((min Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x56) (proj_synd_tup_Int_Int_1 x56)
    (proj_synd_tup_Int_Int_Bool_0 x57) (proj_synd_tup_Int_Int_Bool_1 x57) 
    (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i14 Int)
(declare-var i1 Int)
(declare-var b5 Bool)
(declare-var i18 Int)
(declare-var i17 Int)
(declare-var i2 Int)
(constraint
 (= (min i2 i17) (s1$0 (mk_synd_tup_Int_Int i2 i2) (mk_synd_tup_Int_Int_Bool i17 i18 b5))))
(constraint
 (= (min (min i1 i14) i17)
  (s1$0 (mk_synd_tup_Int_Int (min i1 i14) (max i1 i14)) (mk_synd_tup_Int_Int_Bool i17 i18 b5))))
(check-synth)
