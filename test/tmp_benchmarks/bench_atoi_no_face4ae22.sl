(set-logic DTNIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun odot$0 ((x112 synd_tup_Int_Int) (x113 synd_tup_Int_Int)) Int 
 ((Ix Int) (Ic Int))
 ((Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x112) (proj_synd_tup_Int_Int_1 x112) 
    (proj_synd_tup_Int_Int_0 x113) (proj_synd_tup_Int_Int_1 x113) (- Ix) 
    (+ Ix Ix) (* Ix Ix) (div Ix Ix)))
  (Ic Int ((Constant Int)))))
(synth-fun odot$1 ((x114 synd_tup_Int_Int) (x115 synd_tup_Int_Int)) Int 
 ((Ix Int) (Ic Int))
 ((Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x114) (proj_synd_tup_Int_Int_1 x114) 
    (proj_synd_tup_Int_Int_0 x115) (proj_synd_tup_Int_Int_1 x115) (- Ix) 
    (+ Ix Ix) (* Ix Ix) (div Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i5 Int)
(declare-var i3 Int)
(declare-var i4 Int)
(declare-var i Int)
(declare-var _elim_i20 Int)
(declare-var _elim_i19 Int)
(constraint
 (= _elim_i19 (odot$0 (mk_synd_tup_Int_Int 0 1) (mk_synd_tup_Int_Int _elim_i19 _elim_i20))))
(constraint
 (= _elim_i19
  (odot$0 (mk_synd_tup_Int_Int 0 (odot$1 (mk_synd_tup_Int_Int 0 1) (mk_synd_tup_Int_Int 0 1)))
   (mk_synd_tup_Int_Int _elim_i19 _elim_i20))))
(constraint
 (= (+ (* 10 _elim_i19) i)
  (odot$0 (mk_synd_tup_Int_Int (+ (* 10 0) i) 10) (mk_synd_tup_Int_Int _elim_i19 _elim_i20))))
(constraint
 (= (+ (* 10 _elim_i19) i4)
  (odot$0
   (mk_synd_tup_Int_Int (+ (* 10 0) i4)
    (odot$1 (mk_synd_tup_Int_Int 0 1) (mk_synd_tup_Int_Int i4 10)))
   (mk_synd_tup_Int_Int _elim_i19 _elim_i20))))
(constraint
 (= (+ (* 10 _elim_i19) i3)
  (odot$0
   (mk_synd_tup_Int_Int (+ (* 10 0) i3)
    (odot$1 (mk_synd_tup_Int_Int i3 10) (mk_synd_tup_Int_Int 0 1)))
   (mk_synd_tup_Int_Int _elim_i19 _elim_i20))))
(constraint
 (= (+ (* 10 (+ (* 10 _elim_i19) i5)) i3)
  (odot$0
   (mk_synd_tup_Int_Int (+ (* 10 (+ (* 10 0) i5)) i3)
    (odot$1 (mk_synd_tup_Int_Int i3 10) (mk_synd_tup_Int_Int i5 10)))
   (mk_synd_tup_Int_Int _elim_i19 _elim_i20))))
(check-synth)
