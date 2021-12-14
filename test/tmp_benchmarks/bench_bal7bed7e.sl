(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Bool
 ((mk_synd_tup_Int_Int_Bool (proj_synd_tup_Int_Int_Bool_0 Int) (proj_synd_tup_Int_Int_Bool_1 Int)
   (proj_synd_tup_Int_Int_Bool_2 Bool))))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun odot$1 ((x9 synd_tup_Int_Int_Bool) (x10 synd_tup_Int_Int_Bool)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((min Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Bool_0 x9) (proj_synd_tup_Int_Int_Bool_1 x9)
    (proj_synd_tup_Int_Int_Bool_0 x10) (proj_synd_tup_Int_Int_Bool_1 x10) 
    (- Ix) (+ Ix Ix) (min Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool
   ((proj_synd_tup_Int_Int_Bool_2 x9) (proj_synd_tup_Int_Int_Bool_2 x10) 
    (= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var b0 Bool)
(declare-var b1 Bool)
(declare-var i0 Int)
(declare-var i Int)
(constraint
 (or (not (and (<= i0 i) (and (<= i0 0) (= b1 (>= i0 0)))))
  (= i0 (odot$1 (mk_synd_tup_Int_Int_Bool 0 0 true) (mk_synd_tup_Int_Int_Bool i i0 b1)))))
(constraint
 (or (not (and (<= i0 i) (and (<= i0 0) (= b1 (>= i0 0)))))
  (= (min i0 (ite b0 (+ i 1) (- i 1)))
   (odot$1
    (mk_synd_tup_Int_Int_Bool (ite b0 (+ 0 1) (- 0 1)) (min 0 (ite b0 (+ 0 1) (- 0 1)))
     (and true (>= (ite b0 (+ 0 1) (- 0 1)) 0)))
    (mk_synd_tup_Int_Int_Bool i i0 b1)))))
(check-synth)
