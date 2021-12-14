(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join1$0 ((x37 Int) (x38 synd_tup_Int_Int) (x39 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic x37 (proj_synd_tup_Int_Int_0 x38) (proj_synd_tup_Int_Int_1 x38)
    (proj_synd_tup_Int_Int_0 x39) (proj_synd_tup_Int_Int_1 x39) (- Ix) 
    (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun join1$1 ((x40 Int) (x41 synd_tup_Int_Int) (x42 synd_tup_Int_Int)) Int
 ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic x40 (proj_synd_tup_Int_Int_0 x41) (proj_synd_tup_Int_Int_1 x41)
    (proj_synd_tup_Int_Int_0 x42) (proj_synd_tup_Int_Int_1 x42) (- Ix) 
    (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var p2 Int)
(declare-var _elim_p Int)
(declare-var _elim_i Int)
(declare-var p Int)
(constraint
 (or (not (>= _elim_i 0))
  (= (max (+ _elim_i p) 0)
   (join1$0 p (mk_synd_tup_Int_Int _elim_i _elim_p) (mk_synd_tup_Int_Int 0 0)))))
(constraint
 (or (not (>= _elim_i 0))
  (= (max (+ (max (+ _elim_i p) 0) p2) 0)
   (join1$0 p (mk_synd_tup_Int_Int _elim_i _elim_p) (mk_synd_tup_Int_Int (max (+ 0 p2) 0) p2)))))
(constraint (= p2 (join1$1 p2 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int 0 0))))
(check-synth)
