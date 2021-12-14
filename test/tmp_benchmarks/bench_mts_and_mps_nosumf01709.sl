(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int (proj_synd_tup_Int_Int_Int_0 Int) (proj_synd_tup_Int_Int_Int_1 Int)
   (proj_synd_tup_Int_Int_Int_2 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun oplus$1 ((x99 Int) (x100 synd_tup_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic x99 (proj_synd_tup_Int_Int_Int_0 x100) (proj_synd_tup_Int_Int_Int_1 x100)
    (proj_synd_tup_Int_Int_Int_2 x100) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i Int)
(declare-var p1 Int)
(declare-var p Int)
(constraint (= (max p 0) (oplus$1 p (mk_synd_tup_Int_Int_Int 0 0 0))))
(constraint
 (= (max (+ (max p1 0) p) 0)
  (oplus$1 p (mk_synd_tup_Int_Int_Int (max (+ 0 p1) 0) (max (+ 0 p1) 0) p1))))
(constraint
 (= (max (+ (max (+ (max i 0) p1) 0) p) 0)
  (oplus$1 p
   (mk_synd_tup_Int_Int_Int (max (+ (max (+ 0 p1) 0) i) 0) (max (+ (max (+ 0 i) 0) p1) 0) (+ p1 i)))))
(check-synth)
