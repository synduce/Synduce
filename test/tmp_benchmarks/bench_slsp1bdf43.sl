(set-logic DTLIA)
(declare-datatype synd_tup_Int_Bool
 ((mk_synd_tup_Int_Bool (proj_synd_tup_Int_Bool_0 Int) (proj_synd_tup_Int_Bool_1 Bool))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun f2$0 ((x11 Int) (x12 synd_tup_Int_Bool) (x13 synd_tup_Int_Bool)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix)))
  (Ix Int
   (Ic x11 (proj_synd_tup_Int_Bool_0 x12) (proj_synd_tup_Int_Bool_0 x13) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool
   ((proj_synd_tup_Int_Bool_1 x12) (proj_synd_tup_Int_Bool_1 x13) (= Ix Ix) 
    (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var b0 Bool)
(declare-var i3 Int)
(declare-var i2 Int)
(declare-var i Int)
(constraint
 (= (ite (and (>= i2 0) b0) (+ i3 i2) i3)
  (f2$0 i (mk_synd_tup_Int_Bool i3 b0) (mk_synd_tup_Int_Bool (max 0 i2) (>= i2 0)))))
(check-synth)
