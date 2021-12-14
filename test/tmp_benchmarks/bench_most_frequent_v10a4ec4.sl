(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun join$1 ((x99 Int) (x100 synd_tup_Int_Int) (x101 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix)))
  (Ix Int
   (Ic x99 (proj_synd_tup_Int_Int_0 x100) (proj_synd_tup_Int_Int_1 x100)
    (proj_synd_tup_Int_Int_0 x101) (proj_synd_tup_Int_Int_1 x101) (- Ix) 
    (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i10 Int)
(declare-var i4 Int)
(declare-var i2 Int)
(declare-var i Int)
(constraint
 (or (not (and (< i4 i) (and (<= i i2) (and true true))))
  (= (ite (> (+ (ite (= i4 i2) 1 0) 1) 1) i2 i4)
   (join$1 i (mk_synd_tup_Int_Int 1 i4) (mk_synd_tup_Int_Int 1 i2)))))
(constraint
 (or (not (and (< i10 i) (and (<= i i2) (and true true))))
  (=
   (ite
    (> (+ (+ (ite (= i10 i2) 1 0) 1) 1)
     (ite (> (+ (ite (= i10 i2) 1 0) 1) 1) (+ (ite (= i10 i2) 1 0) 1) 1))
    i2 (ite (> (+ (ite (= i10 i2) 1 0) 1) 1) i2 i10))
   (join$1 i (mk_synd_tup_Int_Int 1 i10)
    (mk_synd_tup_Int_Int (ite (> (+ (ite (= i2 i2) 1 0) 1) 1) (+ (ite (= i2 i2) 1 0) 1) 1)
     (ite (> (+ (ite (= i2 i2) 1 0) 1) 1) i2 i2))))))
(check-synth)
