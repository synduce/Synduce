(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int (proj_synd_tup_Int_Int_Int_0 Int) (proj_synd_tup_Int_Int_Int_1 Int)
   (proj_synd_tup_Int_Int_Int_2 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun oplus$0 ((x18 Int) (x19 Int) (x20 synd_tup_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic x18 x19 (proj_synd_tup_Int_Int_Int_0 x20) (proj_synd_tup_Int_Int_Int_1 x20)
    (proj_synd_tup_Int_Int_Int_2 x20) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i4 Int)
(declare-var i0 Int)
(declare-var i Int)
(constraint
 (= (max (ite (> i4 0) i4 0) i4)
  (oplus$0 i i0
   (mk_synd_tup_Int_Int_Int (ite (> i4 0) i4 0) (ite (> i4 0) i4 0) (ite (> i4 0) i4 0)))))
(check-synth)
