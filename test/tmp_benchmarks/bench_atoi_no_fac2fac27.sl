(set-logic DTNIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun odot$0 ((x69 synd_tup_Int_Int) (x70 synd_tup_Int_Int)) Int
 ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x69) (proj_synd_tup_Int_Int_1 x69) (proj_synd_tup_Int_Int_0 x70)
    (proj_synd_tup_Int_Int_1 x70) (- Ix) (+ Ix Ix) (* Ix Ix) (div Ix Ix) 
    (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun odot$1 ((x71 synd_tup_Int_Int) (x72 synd_tup_Int_Int)) Int
 ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x71) (proj_synd_tup_Int_Int_1 x71) (proj_synd_tup_Int_Int_0 x72)
    (proj_synd_tup_Int_Int_1 x72) (- Ix) (+ Ix Ix) (* Ix Ix) (div Ix Ix) 
    (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
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
