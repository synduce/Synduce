(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join1$0 ((x79 Int) (x80 synd_tup_Int_Int) (x81 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic x79 (proj_synd_tup_Int_Int_0 x80) (proj_synd_tup_Int_Int_1 x80)
    (proj_synd_tup_Int_Int_0 x81) (proj_synd_tup_Int_Int_1 x81) (- Ix) 
    (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var p8 Int)
(declare-var p2 Int)
(declare-var _elim_i7 Int)
(declare-var _elim_i6 Int)
(declare-var p Int)
(constraint
 (or (not (>= _elim_i6 0))
  (= (max (+ _elim_i6 p) 0)
   (join1$0 p (mk_synd_tup_Int_Int _elim_i6 _elim_i7) (mk_synd_tup_Int_Int 0 0)))))
(constraint
 (or (not (>= _elim_i6 0))
  (= (max (+ (max (+ _elim_i6 p) 0) p2) 0)
   (join1$0 p (mk_synd_tup_Int_Int _elim_i6 _elim_i7) (mk_synd_tup_Int_Int (max (+ 0 p2) 0) p2)))))
(constraint
 (or (not (>= _elim_i6 0))
  (= (max (+ (max (+ (max (+ _elim_i6 p) 0) p2) 0) p8) 0)
   (join1$0 p (mk_synd_tup_Int_Int _elim_i6 _elim_i7)
    (mk_synd_tup_Int_Int (max (+ (max (+ 0 p2) 0) p8) 0) (+ p2 p8))))))
(check-synth)
