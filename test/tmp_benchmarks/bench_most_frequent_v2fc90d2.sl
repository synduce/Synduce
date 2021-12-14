(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun join$1 ((x54 Int) (x55 Int) (x56 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix)))
  (Ix Int
   (Ic x54 x55 (proj_synd_tup_Int_Int_0 x56) (proj_synd_tup_Int_Int_1 x56) 
    (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i4 Int)
(declare-var i2 Int)
(declare-var i Int)
(constraint
 (or (not (and (not (= i2 i)) true))
  (= (ite (> (+ (ite (= i2 i) 1 0) 1) 1) i i2) (join$1 i 1 (mk_synd_tup_Int_Int 1 i2)))))
(constraint
 (or (not (and (not (= i2 i)) true))
  (= (ite (> (+ (+ (ite (= i2 i) 1 0) (ite (= i2 i) 1 0)) 1) 2) i i2)
   (join$1 i 1
    (mk_synd_tup_Int_Int (ite (> (+ (ite (= i2 i2) 1 0) 1) 1) (+ (ite (= i2 i2) 1 0) 1) 1)
     (ite (> (+ (ite (= i2 i2) 1 0) 1) 1) i2 i2))))))
(constraint
 (or (not (and (not (= i4 i)) true))
  (=
   (ite
    (> (+ (+ (ite (= i4 i) 1 0) 1) 1)
     (ite (> (+ (ite (= i4 i) 1 0) 1) 1) (+ (ite (= i4 i) 1 0) 1) 1))
    i (ite (> (+ (ite (= i4 i) 1 0) 1) 1) i i4))
   (join$1 i (+ 1 1) (mk_synd_tup_Int_Int 1 i4)))))
(check-synth)
