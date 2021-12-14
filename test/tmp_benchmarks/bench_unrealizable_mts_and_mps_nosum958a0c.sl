(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun oplus$0 ((x16 Int) (x17 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic x16 (proj_synd_tup_Int_Int_0 x17) (proj_synd_tup_Int_Int_1 x17) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var p1 Int)
(declare-var p Int)
(constraint (= (max p 0) (oplus$0 p (mk_synd_tup_Int_Int 0 0))))
(constraint
 (= (max (+ (max p 0) p1) 0) (oplus$0 p (mk_synd_tup_Int_Int (max (+ 0 p1) 0) (max (+ 0 p1) 0)))))
(check-synth)
