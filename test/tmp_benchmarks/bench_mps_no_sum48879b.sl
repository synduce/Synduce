(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun oplus$0 ((x90 synd_tup_Int_Int) (x91 Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x90) (proj_synd_tup_Int_Int_1 x90) x91 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun oplus$1 ((x92 synd_tup_Int_Int) (x93 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x92) (proj_synd_tup_Int_Int_1 x92) x93 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var p3 Int)
(declare-var p1 Int)
(declare-var p Int)
(constraint (= (max p 0) (oplus$0 (mk_synd_tup_Int_Int 0 1) p)))
(constraint
 (= (max (+ (max p1 0) p) 0)
  (oplus$0 (mk_synd_tup_Int_Int (max p 0) (oplus$1 (mk_synd_tup_Int_Int 0 1) p)) p1)))
(constraint
 (= (max (+ (max (+ (max p3 0) p1) 0) p) 0)
  (oplus$0
   (mk_synd_tup_Int_Int (max (+ (max p1 0) p) 0)
    (oplus$1 (mk_synd_tup_Int_Int (max p 0) (oplus$1 (mk_synd_tup_Int_Int 0 1) p)) p1))
   p3)))
(check-synth)
