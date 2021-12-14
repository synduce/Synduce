(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int (proj_synd_tup_Int_Int_Int_0 Int) (proj_synd_tup_Int_Int_Int_1 Int)
   (proj_synd_tup_Int_Int_Int_2 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun s0$2 () Int)
(synth-fun oplus$0 ((x51 Int) (x52 synd_tup_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic x51 (proj_synd_tup_Int_Int_Int_0 x52) (proj_synd_tup_Int_Int_Int_1 x52)
    (proj_synd_tup_Int_Int_Int_2 x52) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun oplus$1 ((x53 Int) (x54 synd_tup_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic x53 (proj_synd_tup_Int_Int_Int_0 x54) (proj_synd_tup_Int_Int_Int_1 x54)
    (proj_synd_tup_Int_Int_Int_2 x54) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun oplus$2 ((x55 Int) (x56 synd_tup_Int_Int_Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic x55 (proj_synd_tup_Int_Int_Int_0 x56) (proj_synd_tup_Int_Int_Int_1 x56)
    (proj_synd_tup_Int_Int_Int_2 x56) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var p1 Int)
(declare-var p Int)
(constraint (= (max p 0) (oplus$0 p (mk_synd_tup_Int_Int_Int 0 0 s0$2))))
(constraint (= (max p 0) (oplus$1 p (mk_synd_tup_Int_Int_Int 0 0 s0$2))))
(constraint
 (= (max (+ (max p 0) p1) 0)
  (oplus$0 p (mk_synd_tup_Int_Int_Int (max (+ 0 p1) 0) (max (+ 0 p1) 0) p1))))
(constraint
 (= (max (+ (max p1 0) p) 0)
  (oplus$1 p (mk_synd_tup_Int_Int_Int (max (+ 0 p1) 0) (max (+ 0 p1) 0) p1))))
(constraint (= p1 (oplus$2 p1 (mk_synd_tup_Int_Int_Int 0 0 s0$2))))
(check-synth)
