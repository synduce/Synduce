(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join$0 ((x69 synd_tup_Int_Int) (x70 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x69) (proj_synd_tup_Int_Int_1 x69) (proj_synd_tup_Int_Int_0 x70)
    (proj_synd_tup_Int_Int_1 x70) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun join$1 ((x71 synd_tup_Int_Int) (x72 synd_tup_Int_Int)) Int
 ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x71) (proj_synd_tup_Int_Int_1 x71) (proj_synd_tup_Int_Int_0 x72)
    (proj_synd_tup_Int_Int_1 x72) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i6 Int)
(declare-var i Int)
(declare-var _elim_i8 Int)
(declare-var _elim_i7 Int)
(constraint
 (or (not (>= _elim_i7 0))
  (= _elim_i7 (join$0 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int _elim_i7 _elim_i8)))))
(constraint
 (or (not (>= _elim_i7 0))
  (= _elim_i7
   (join$0 (mk_synd_tup_Int_Int 0 (join$1 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int 0 0)))
    (mk_synd_tup_Int_Int _elim_i7 _elim_i8)))))
(constraint
 (or (not (>= _elim_i7 0))
  (= (max (+ _elim_i7 i) 0)
   (join$0 (mk_synd_tup_Int_Int (max (+ 0 i) 0) i) (mk_synd_tup_Int_Int _elim_i7 _elim_i8)))))
(constraint
 (or (not (>= _elim_i7 0))
  (= (max (+ _elim_i7 i6) 0)
   (join$0 (mk_synd_tup_Int_Int (max (+ 0 i6) 0) i6) (mk_synd_tup_Int_Int _elim_i7 _elim_i8)))))
(constraint (= i6 (join$1 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int (max (+ 0 i6) 0) i6))))
(check-synth)
