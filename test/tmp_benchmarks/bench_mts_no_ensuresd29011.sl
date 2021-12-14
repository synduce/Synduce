(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join$0 ((x6 synd_tup_Int_Int) (x7 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x6) (proj_synd_tup_Int_Int_1 x6) (proj_synd_tup_Int_Int_0 x7)
    (proj_synd_tup_Int_Int_1 x7) (- Ix) (+ Ix Ix) (max Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i Int)
(declare-var i1 Int)
(declare-var i0 Int)
(constraint (= i0 (join$0 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int i0 i1))))
(constraint
 (= (max i0 (+ i1 i))
  (join$0 (mk_synd_tup_Int_Int (max 0 (+ 0 i)) (+ 0 i)) (mk_synd_tup_Int_Int i0 i1))))
(check-synth)
