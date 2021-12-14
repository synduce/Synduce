(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Bool
 ((mk_synd_tup_Int_Int_Bool (proj_synd_tup_Int_Int_Bool_0 Int) (proj_synd_tup_Int_Int_Bool_1 Int)
   (proj_synd_tup_Int_Int_Bool_2 Bool))))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun f2$2 ((x5 synd_tup_Int_Int_Bool) (x6 synd_tup_Int_Int_Bool)) Bool
 ((IStart Bool) (Ipred Bool) (Ix Int) (Ic Int))
 ((IStart Bool ((and Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Int_Int_Bool_2 x5) (proj_synd_tup_Int_Int_Bool_2 x6) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred) (= Ix Ix) (> Ix Ix) 
    (>= Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Bool_0 x5) (proj_synd_tup_Int_Int_Bool_1 x5)
    (proj_synd_tup_Int_Int_Bool_0 x6) (proj_synd_tup_Int_Int_Bool_1 x6) 
    (- Ix) (+ Ix Ix) (min Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var b0 Bool)
(declare-var b1 Bool)
(declare-var i0 Int)
(declare-var i Int)
(constraint (= b1 (f2$2 (mk_synd_tup_Int_Int_Bool 0 0 true) (mk_synd_tup_Int_Int_Bool i i0 b1))))
(constraint
 (= (and b1 (>= (ite b0 (+ i 1) (- i 1)) 0))
  (f2$2
   (mk_synd_tup_Int_Int_Bool (ite b0 (+ 0 1) (- 0 1)) (min 0 (ite b0 (+ 0 1) (- 0 1)))
    (and true (>= (ite b0 (+ 0 1) (- 0 1)) 0)))
   (mk_synd_tup_Int_Int_Bool i i0 b1))))
(check-synth)
