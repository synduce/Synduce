(set-logic DTNIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun odot$0 ((x65 synd_tup_Int_Int) (x66 synd_tup_Int_Int)) Int 
 ((Ix Int) (Ic Int))
 ((Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x65) (proj_synd_tup_Int_Int_1 x65) (proj_synd_tup_Int_Int_0 x66)
    (proj_synd_tup_Int_Int_1 x66) (- Ix) (+ Ix Ix) (* Ix Ix) (div Ix Ix)))
  (Ic Int ((Constant Int)))))
(synth-fun odot$1 ((x67 synd_tup_Int_Int) (x68 synd_tup_Int_Int)) Int 
 ((Ix Int) (Ic Int))
 ((Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x67) (proj_synd_tup_Int_Int_1 x67) (proj_synd_tup_Int_Int_0 x68)
    (proj_synd_tup_Int_Int_1 x68) (- Ix) (+ Ix Ix) (* Ix Ix) (div Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i4 Int)
(declare-var i Int)
(declare-var _elim_i8 Int)
(declare-var _elim_i7 Int)
(constraint
 (= _elim_i7 (odot$0 (mk_synd_tup_Int_Int 0 1) (mk_synd_tup_Int_Int _elim_i7 _elim_i8))))
(constraint
 (= _elim_i7
  (odot$0 (mk_synd_tup_Int_Int 0 (odot$1 (mk_synd_tup_Int_Int 0 1) (mk_synd_tup_Int_Int 0 1)))
   (mk_synd_tup_Int_Int _elim_i7 _elim_i8))))
(constraint
 (= (+ (* 10 _elim_i7) i)
  (odot$0 (mk_synd_tup_Int_Int (+ (* 10 0) i) 10) (mk_synd_tup_Int_Int _elim_i7 _elim_i8))))
(constraint
 (= (+ (* 10 _elim_i7) i4)
  (odot$0
   (mk_synd_tup_Int_Int (+ (* 10 0) i4)
    (odot$1 (mk_synd_tup_Int_Int 0 1) (mk_synd_tup_Int_Int i4 10)))
   (mk_synd_tup_Int_Int _elim_i7 _elim_i8))))
(check-synth)
