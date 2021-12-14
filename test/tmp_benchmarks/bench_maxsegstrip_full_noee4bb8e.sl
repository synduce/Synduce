(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun s1$2 ((x63 synd_tup_Int_Int_Int_Int) (x64 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x63) (proj_synd_tup_Int_Int_Int_Int_1 x63)
    (proj_synd_tup_Int_Int_Int_Int_2 x63) (proj_synd_tup_Int_Int_Int_Int_3 x63)
    (proj_synd_tup_Int_Int_Int_Int_0 x64) (proj_synd_tup_Int_Int_Int_Int_1 x64)
    (proj_synd_tup_Int_Int_Int_Int_2 x64) (proj_synd_tup_Int_Int_Int_Int_3 x64) 
    (- Ix) (+ Ix Ix) (max Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i34 Int)
(declare-var i22 Int)
(declare-var i42 Int)
(declare-var i41 Int)
(declare-var i40 Int)
(declare-var i39 Int)
(declare-var i2 Int)
(constraint
 (= (max (+ i42 i2) i41)
  (s1$2 (mk_synd_tup_Int_Int_Int_Int i39 i40 i41 i42)
   (mk_synd_tup_Int_Int_Int_Int (max i2 0) (max i2 0) (max i2 0) i2))))
(constraint
 (= (max (+ (+ i42 i34) i22) (max (+ i42 i34) i41))
  (s1$2 (mk_synd_tup_Int_Int_Int_Int i39 i40 i41 i42)
   (mk_synd_tup_Int_Int_Int_Int (max (+ (max i34 0) i22) 0)
    (max (max i34 0) (max (+ (max i34 0) i22) 0)) (max (+ i34 i22) (max i34 0)) 
    (+ i34 i22)))))
(check-synth)
