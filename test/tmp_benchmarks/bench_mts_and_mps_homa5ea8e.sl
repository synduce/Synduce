(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int (proj_synd_tup_Int_Int_Int_0 Int) (proj_synd_tup_Int_Int_Int_1 Int)
   (proj_synd_tup_Int_Int_Int_2 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun odot$1 ((x9 synd_tup_Int_Int_Int) (x10 synd_tup_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_0 x9) (proj_synd_tup_Int_Int_Int_1 x9)
    (proj_synd_tup_Int_Int_Int_2 x9) (proj_synd_tup_Int_Int_Int_0 x10)
    (proj_synd_tup_Int_Int_Int_1 x10) (proj_synd_tup_Int_Int_Int_2 x10) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var p4 Int)
(declare-var i1 Int)
(declare-var i0 Int)
(declare-var i Int)
(constraint
 (or (not (and (>= i1 0) (and (>= i0 0) (and (>= i0 i) (>= i1 i)))))
  (= i0 (odot$1 (mk_synd_tup_Int_Int_Int 0 0 0) (mk_synd_tup_Int_Int_Int i i0 i1)))))
(constraint
 (or (not (and (>= i1 0) (and (>= i0 0) (and (>= i0 i) (>= i1 i)))))
  (= (max (+ i0 p4) 0)
   (odot$1 (mk_synd_tup_Int_Int_Int (+ 0 p4) (max (+ 0 p4) 0) (max 0 (+ 0 p4)))
    (mk_synd_tup_Int_Int_Int i i0 i1)))))
(check-synth)
