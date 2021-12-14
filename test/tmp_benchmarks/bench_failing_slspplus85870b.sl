(set-logic DTLIA)
(declare-datatype synd_tup_Int_Bool
 ((mk_synd_tup_Int_Bool (proj_synd_tup_Int_Bool_0 Int) (proj_synd_tup_Int_Bool_1 Bool))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun f2$0 ((x22 synd_tup_Int_Bool) (x23 synd_tup_Int_Bool)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Bool_0 x22) (proj_synd_tup_Int_Bool_0 x23) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool
   ((proj_synd_tup_Int_Bool_1 x22) (proj_synd_tup_Int_Bool_1 x23) (= Ix Ix) 
    (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var b2 Bool)
(declare-var i992 Int)
(declare-var i4 Int)
(declare-var i Int)
(constraint
 (or
  (not
   (and
    (and (and (>= i992 0) (= b2 (= i992 0)))
     (and (or (not (<= i 0)) (> i (max i4 (+ i i)))) (or (not (<= i 0)) (> i992 i))))
    (not (<= i 0))))
  (= (ite (and (>= i4 0) b2) (+ i992 i4) 0)
   (f2$0 (mk_synd_tup_Int_Bool i992 b2) (mk_synd_tup_Int_Bool (max 0 i4) (>= i4 0))))))
(check-synth)
