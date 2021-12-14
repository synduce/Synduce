(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join$0 ((x20 synd_tup_Int_Int) (x21 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x20) (proj_synd_tup_Int_Int_1 x20) (proj_synd_tup_Int_Int_0 x21)
    (proj_synd_tup_Int_Int_1 x21) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i9 Int)
(declare-var i5 Int)
(declare-var p4 Int)
(declare-var i13 Int)
(declare-var i12 Int)
(constraint
 (or (not (and (>= i12 0) (>= i12 i13)))
  (= i12 (join$0 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int i12 i13)))))
(constraint
 (or (not (and (>= i12 0) (>= i12 i13)))
  (= (max i12 (+ i13 p4))
   (join$0 (mk_synd_tup_Int_Int (max 0 (+ 0 p4)) (+ 0 p4)) (mk_synd_tup_Int_Int i12 i13)))))
(constraint
 (or (not (and (>= i12 0) (>= i12 i13)))
  (= (max i12 (+ i13 i5))
   (join$0 (mk_synd_tup_Int_Int (max 0 (+ 0 i5)) (+ 0 i5)) (mk_synd_tup_Int_Int i12 i13)))))
(constraint
 (or (not (and (>= i12 0) (>= i12 i13)))
  (= (max (max i12 (+ i13 i9)) (+ (+ i13 i9) i5))
   (join$0 (mk_synd_tup_Int_Int (max (max 0 (+ 0 i9)) (+ (+ 0 i9) i5)) (+ (+ 0 i9) i5))
    (mk_synd_tup_Int_Int i12 i13)))))
(check-synth)
