(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun s0$1 () Int)
(synth-fun join1$0 ((x43 Int) (x44 synd_tup_Int_Int) (x45 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic x43 (proj_synd_tup_Int_Int_0 x44) (proj_synd_tup_Int_Int_1 x44)
    (proj_synd_tup_Int_Int_0 x45) (proj_synd_tup_Int_Int_1 x45) (- Ix) 
    (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun join1$1 ((x46 Int) (x47 synd_tup_Int_Int) (x48 synd_tup_Int_Int)) Int
 ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic x46 (proj_synd_tup_Int_Int_0 x47) (proj_synd_tup_Int_Int_1 x47)
    (proj_synd_tup_Int_Int_0 x48) (proj_synd_tup_Int_Int_1 x48) (- Ix) 
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
   (join1$0 p (mk_synd_tup_Int_Int _elim_i _elim_p) (mk_synd_tup_Int_Int 0 s0$1)))))
(constraint
 (or (not (>= _elim_i 0))
  (= (max (+ (max (+ _elim_i p) 0) p2) 0)
   (join1$0 p (mk_synd_tup_Int_Int _elim_i _elim_p) (mk_synd_tup_Int_Int (max (+ 0 p2) 0) p2)))))
(constraint (= p2 (join1$1 p2 (mk_synd_tup_Int_Int 0 s0$1) (mk_synd_tup_Int_Int 0 s0$1))))
(check-synth)
