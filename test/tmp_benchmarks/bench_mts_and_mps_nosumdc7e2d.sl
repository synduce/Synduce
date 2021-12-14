(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int (proj_synd_tup_Int_Int_Int_0 Int) (proj_synd_tup_Int_Int_Int_1 Int)
   (proj_synd_tup_Int_Int_Int_2 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun oplus$0 ((x45 Int) (x46 synd_tup_Int_Int_Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic x45 (proj_synd_tup_Int_Int_Int_0 x46) (proj_synd_tup_Int_Int_Int_1 x46)
    (proj_synd_tup_Int_Int_Int_2 x46) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun oplus$1 ((x47 Int) (x48 synd_tup_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic x47 (proj_synd_tup_Int_Int_Int_0 x48) (proj_synd_tup_Int_Int_Int_1 x48)
    (proj_synd_tup_Int_Int_Int_2 x48) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun oplus$2 ((x49 Int) (x50 synd_tup_Int_Int_Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic x49 (proj_synd_tup_Int_Int_Int_0 x50) (proj_synd_tup_Int_Int_Int_1 x50)
    (proj_synd_tup_Int_Int_Int_2 x50) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var p1 Int)
(declare-var p Int)
(constraint (= (max p 0) (oplus$0 p (mk_synd_tup_Int_Int_Int 0 0 0))))
(constraint (= (max p 0) (oplus$1 p (mk_synd_tup_Int_Int_Int 0 0 0))))
(constraint
 (= (max (+ (max p 0) p1) 0)
  (oplus$0 p (mk_synd_tup_Int_Int_Int (max (+ 0 p1) 0) (max (+ 0 p1) 0) p1))))
(constraint
 (= (max (+ (max p1 0) p) 0)
  (oplus$1 p (mk_synd_tup_Int_Int_Int (max (+ 0 p1) 0) (max (+ 0 p1) 0) p1))))
(constraint (= p1 (oplus$2 p1 (mk_synd_tup_Int_Int_Int 0 0 0))))
(check-synth)
