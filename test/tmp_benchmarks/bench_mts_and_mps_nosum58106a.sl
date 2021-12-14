(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int (proj_synd_tup_Int_Int_Int_0 Int) (proj_synd_tup_Int_Int_Int_1 Int)
   (proj_synd_tup_Int_Int_Int_2 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun oplus$0 ((x39 Int) (x40 synd_tup_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic x39 (proj_synd_tup_Int_Int_Int_0 x40) (proj_synd_tup_Int_Int_Int_1 x40)
    (proj_synd_tup_Int_Int_Int_2 x40) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun oplus$1 ((x41 Int) (x42 synd_tup_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic x41 (proj_synd_tup_Int_Int_Int_0 x42) (proj_synd_tup_Int_Int_Int_1 x42)
    (proj_synd_tup_Int_Int_Int_2 x42) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun oplus$2 ((x43 Int) (x44 synd_tup_Int_Int_Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic x43 (proj_synd_tup_Int_Int_Int_0 x44) (proj_synd_tup_Int_Int_Int_1 x44)
    (proj_synd_tup_Int_Int_Int_2 x44) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var p1 Int)
(declare-var p Int)
(constraint (= (max p 0) (oplus$0 p (mk_synd_tup_Int_Int_Int 0 0 1))))
(constraint (= (max p 0) (oplus$1 p (mk_synd_tup_Int_Int_Int 0 0 1))))
(constraint
 (= (max (+ (max p 0) p1) 0)
  (oplus$0 p (mk_synd_tup_Int_Int_Int (max (+ 0 p1) 0) (max (+ 0 p1) 0) p1))))
(constraint
 (= (max (+ (max p1 0) p) 0)
  (oplus$1 p (mk_synd_tup_Int_Int_Int (max (+ 0 p1) 0) (max (+ 0 p1) 0) p1))))
(constraint (= p1 (oplus$2 p1 (mk_synd_tup_Int_Int_Int 0 0 1))))
(check-synth)
