(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun s1$0 ((x18 synd_tup_Int_Int) (x19 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x18) (proj_synd_tup_Int_Int_1 x18) (proj_synd_tup_Int_Int_0 x19)
    (proj_synd_tup_Int_Int_1 x19) (- Ix) (+ Ix Ix) (max Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i20 Int)
(declare-var i10 Int)
(declare-var i24 Int)
(declare-var i23 Int)
(declare-var i2 Int)
(constraint
 (= (max (+ i2 i23) 0) (s1$0 (mk_synd_tup_Int_Int i23 i24) (mk_synd_tup_Int_Int (max 0 i2) i2))))
(constraint
 (= (max (+ i10 (max (+ i20 i23) 0)) 0)
  (s1$0 (mk_synd_tup_Int_Int i23 i24)
   (mk_synd_tup_Int_Int (max (+ i10 (max 0 i20)) 0) (+ i20 i10)))))
(check-synth)
