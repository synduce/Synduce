(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun join$0 ((x119 Int) (x120 synd_tup_Int_Int) (x121 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix)))
  (Ix Int
   (Ic x119 (proj_synd_tup_Int_Int_0 x120) (proj_synd_tup_Int_Int_1 x120)
    (proj_synd_tup_Int_Int_0 x121) (proj_synd_tup_Int_Int_1 x121) (- Ix) 
    (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i10 Int)
(declare-var i4 Int)
(declare-var i2 Int)
(declare-var i Int)
(constraint
 (or (not (and (< i4 i) (and (<= i i2) (and true true))))
  (= (ite (> (+ (ite (= i4 i2) 1 0) 1) 1) (+ (ite (= i4 i2) 1 0) 1) 1)
   (join$0 i (mk_synd_tup_Int_Int 1 i4) (mk_synd_tup_Int_Int 1 i2)))))
(constraint
 (or (not (and (< i4 i) (and (<= i i2) (and true true))))
  (=
   (ite (> (+ (+ (ite (= i4 i2) 1 0) (ite (= i4 i2) 1 0)) 1) 2)
    (+ (+ (ite (= i4 i2) 1 0) (ite (= i4 i2) 1 0)) 1) 2)
   (join$0 i
    (mk_synd_tup_Int_Int (ite (> (+ (ite (= i4 i4) 1 0) 1) 1) (+ (ite (= i4 i4) 1 0) 1) 1)
     (ite (> (+ (ite (= i4 i4) 1 0) 1) 1) i4 i4))
    (mk_synd_tup_Int_Int 1 i2)))))
(constraint
 (or (not (and (< i10 i) (and (<= i i2) (and true true))))
  (=
   (ite
    (> (+ (+ (ite (= i10 i2) 1 0) 1) 1)
     (ite (> (+ (ite (= i10 i2) 1 0) 1) 1) (+ (ite (= i10 i2) 1 0) 1) 1))
    (+ (+ (ite (= i10 i2) 1 0) 1) 1)
    (ite (> (+ (ite (= i10 i2) 1 0) 1) 1) (+ (ite (= i10 i2) 1 0) 1) 1))
   (join$0 i (mk_synd_tup_Int_Int 1 i10)
    (mk_synd_tup_Int_Int (ite (> (+ (ite (= i2 i2) 1 0) 1) 1) (+ (ite (= i2 i2) 1 0) 1) 1)
     (ite (> (+ (ite (= i2 i2) 1 0) 1) 1) i2 i2))))))
(check-synth)
