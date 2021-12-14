(set-logic DTLIA)
(declare-datatype synd_tup_Int_Bool
 ((mk_synd_tup_Int_Bool (proj_synd_tup_Int_Bool_0 Int) (proj_synd_tup_Int_Bool_1 Bool))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun f1$0 ((x15 synd_tup_Int_Bool)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix)))
  (Ix Int (Ic (proj_synd_tup_Int_Bool_0 x15) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool
   ((proj_synd_tup_Int_Bool_1 x15) (= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred)
    (or Ipred Ipred)))))
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
    (<= i 0)))
  (= (ite (and (>= i4 0) b2) (+ i992 i4) 0) (f1$0 (mk_synd_tup_Int_Bool i992 b2)))))
(check-synth)
