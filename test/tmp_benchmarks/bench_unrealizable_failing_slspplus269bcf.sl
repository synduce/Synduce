(set-logic DTLIA)
(declare-datatype synd_tup_Int_Bool
 ((mk_synd_tup_Int_Bool (proj_synd_tup_Int_Bool_0 Int) (proj_synd_tup_Int_Bool_1 Bool))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun f1$1 ((x48 synd_tup_Int_Bool)) Bool ((IStart Bool) (Ipred Bool) (Ix Int) (Ic Int))
 ((IStart Bool ((and Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Int_Bool_1 x48) (not Ipred) (and Ipred Ipred) (or Ipred Ipred) 
    (= Ix Ix) (> Ix Ix) (>= Ix Ix)))
  (Ix Int (Ic (proj_synd_tup_Int_Bool_0 x48) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i1005 Int)
(declare-var i3 Int)
(declare-var b6 Bool)
(declare-var i1009 Int)
(declare-var i4 Int)
(declare-var i Int)
(constraint
 (or
  (not
   (and
    (and (and (>= i1009 0) (= b6 (= i1009 0)))
     (and (or (not (<= i 0)) (> i (max i4 (+ i i)))) (or (not (<= i 0)) (> i1009 i))))
    (<= i 0)))
  (= (and b6 (>= i4 0)) (f1$1 (mk_synd_tup_Int_Bool i1009 b6)))))
(constraint
 (or (not (and (and (>= i1009 0) (= b6 (= i1009 0))) (<= i 0)))
  (= (and b6 (>= (+ i3 i1005) 0)) (f1$1 (mk_synd_tup_Int_Bool i1009 b6)))))
(check-synth)
