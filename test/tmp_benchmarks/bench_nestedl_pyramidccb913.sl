(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun s0$0 ((x40 synd_tup_Int_Int) (x41 Bool)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((min Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x40) (proj_synd_tup_Int_Int_1 x40) (- Ix) 
    (+ Ix Ix) (min Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool (x41 (= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i8 Int)
(declare-var i Int)
(declare-var i0 Int)
(constraint (or (not true) (= i0 (s0$0 (mk_synd_tup_Int_Int i0 i0) true))))
(constraint (or (not true) (= (min i i8) (s0$0 (mk_synd_tup_Int_Int (min i i8) (max i i8)) true))))
(check-synth)
