(set-logic DTNIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun odot$0 ((x76 synd_tup_Int_Int) (x77 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((+ Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x76) (proj_synd_tup_Int_Int_1 x76) (proj_synd_tup_Int_Int_0 x77)
    (proj_synd_tup_Int_Int_1 x77) (- Ix) (+ Ix Ix) (* Ix Ix) (div Ix Ix)))
  (Ic Int ((Constant Int)))))
(synth-fun odot$1 ((x78 synd_tup_Int_Int) (x79 synd_tup_Int_Int)) Int 
 ((Ix Int) (Ic Int))
 ((Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x78) (proj_synd_tup_Int_Int_1 x78) (proj_synd_tup_Int_Int_0 x79)
    (proj_synd_tup_Int_Int_1 x79) (- Ix) (+ Ix Ix) (* Ix Ix) (div Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i6 Int)
(declare-var i Int)
(declare-var x Int)
(declare-var _elim_i8 Int)
(declare-var _elim_i7 Int)
(constraint
 (= _elim_i7 (odot$0 (mk_synd_tup_Int_Int 0 1) (mk_synd_tup_Int_Int _elim_i7 _elim_i8))))
(constraint
 (= _elim_i7
  (odot$0 (mk_synd_tup_Int_Int 0 (odot$1 (mk_synd_tup_Int_Int 0 1) (mk_synd_tup_Int_Int 0 1)))
   (mk_synd_tup_Int_Int _elim_i7 _elim_i8))))
(constraint
 (= (+ (* x _elim_i7) i)
  (odot$0 (mk_synd_tup_Int_Int (+ (* x 0) i) x) (mk_synd_tup_Int_Int _elim_i7 _elim_i8))))
(constraint
 (= (+ (* x _elim_i7) i6)
  (odot$0 (mk_synd_tup_Int_Int (+ (* x 0) i6) x) (mk_synd_tup_Int_Int _elim_i7 _elim_i8))))
(constraint (= x (odot$1 (mk_synd_tup_Int_Int 0 1) (mk_synd_tup_Int_Int (+ (* x 0) i6) x))))
(check-synth)
