(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join$0 ((x27 synd_tup_Int_Int) (x28 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x27) (proj_synd_tup_Int_Int_1 x27) (proj_synd_tup_Int_Int_0 x28)
    (proj_synd_tup_Int_Int_1 x28) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i Int)
(declare-var _elim_i0 Int)
(declare-var _elim_i Int)
(constraint
 (or (not (>= _elim_i 0))
  (= _elim_i (join$0 (mk_synd_tup_Int_Int 0 1) (mk_synd_tup_Int_Int _elim_i _elim_i0)))))
(constraint
 (or (not (>= _elim_i 0))
  (= (max (+ _elim_i i) 0)
   (join$0 (mk_synd_tup_Int_Int (max (+ 0 i) 0) i) (mk_synd_tup_Int_Int _elim_i _elim_i0)))))
(check-synth)
