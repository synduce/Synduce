(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun joinr$0 ((x6 Int) (x7 synd_tup_Int_Int) (x8 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix)))
  (Ix Int
   (Ic x6 (proj_synd_tup_Int_Int_0 x7) (proj_synd_tup_Int_Int_1 x7) (proj_synd_tup_Int_Int_0 x8)
    (proj_synd_tup_Int_Int_1 x8) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i7 Int)
(declare-var i14 Int)
(declare-var i13 Int)
(declare-var i Int)
(constraint
 (or (not (and (>= i14 0) (>= i14 i13)))
  (= (+ i13 i) (joinr$0 i (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int i13 i14)))))
(constraint
 (or (not (and (>= i14 0) (>= i14 i13)))
  (= (+ (+ i13 i) i7)
   (joinr$0 i (mk_synd_tup_Int_Int (+ 0 i7) (max (+ 0 i7) 0)) (mk_synd_tup_Int_Int i13 i14)))))
(check-synth)
