(set-logic DTLIA)
(declare-datatype synd_tup_Int_Bool
 ((mk_synd_tup_Int_Bool (proj_synd_tup_Int_Bool_0 Int) (proj_synd_tup_Int_Bool_1 Bool))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun f2$0 ((x25 Int) (x26 synd_tup_Int_Bool) (x27 synd_tup_Int_Bool)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix)))
  (Ix Int
   (Ic x25 (proj_synd_tup_Int_Bool_0 x26) (proj_synd_tup_Int_Bool_0 x27) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool
   ((proj_synd_tup_Int_Bool_1 x26) (proj_synd_tup_Int_Bool_1 x27) (= Ix Ix) 
    (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var b2 Bool)
(declare-var i655 Int)
(declare-var i2 Int)
(declare-var i Int)
(constraint
 (or
  (not
   (and
    (and (>= i655 0)
     (and (or (not (<= i 0)) (> i (ite (= i i655) i i2))) (or (not (<= i 0)) (> i655 i))))
    (not (<= i 0))))
  (= (ite (and (>= i2 0) b2) (+ i655 i2) i655)
   (f2$0 i (mk_synd_tup_Int_Bool i655 b2) (mk_synd_tup_Int_Bool (max 0 i2) (>= i2 0))))))
(check-synth)
