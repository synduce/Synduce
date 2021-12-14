(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int (proj_synd_tup_Int_Int_Int_0 Int) (proj_synd_tup_Int_Int_Int_1 Int)
   (proj_synd_tup_Int_Int_Int_2 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun odot$1 ((x25 synd_tup_Int_Int_Int) (x26 synd_tup_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_0 x25) (proj_synd_tup_Int_Int_Int_1 x25)
    (proj_synd_tup_Int_Int_Int_2 x25) (proj_synd_tup_Int_Int_Int_0 x26)
    (proj_synd_tup_Int_Int_Int_1 x26) (proj_synd_tup_Int_Int_Int_2 x26) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i13 Int)
(declare-var i8 Int)
(declare-var p4 Int)
(declare-var i19 Int)
(declare-var i18 Int)
(declare-var i17 Int)
(constraint
 (or (not (and (>= i19 0) (and (>= i18 0) (and (>= i18 i17) (>= i19 i17)))))
  (= i18 (odot$1 (mk_synd_tup_Int_Int_Int 0 0 0) (mk_synd_tup_Int_Int_Int i17 i18 i19)))))
(constraint
 (or (not (and (>= i19 0) (and (>= i18 0) (and (>= i18 i17) (>= i19 i17)))))
  (= (max (+ i18 p4) 0)
   (odot$1 (mk_synd_tup_Int_Int_Int (+ 0 p4) (max (+ 0 p4) 0) (max 0 (+ 0 p4)))
    (mk_synd_tup_Int_Int_Int i17 i18 i19)))))
(constraint
 (or (not (and (>= i19 0) (and (>= i18 0) (and (>= i18 i17) (>= i19 i17)))))
  (= (max (+ i18 i8) 0)
   (odot$1 (mk_synd_tup_Int_Int_Int (+ 0 i8) (max (+ 0 i8) 0) (max 0 (+ 0 i8)))
    (mk_synd_tup_Int_Int_Int i17 i18 i19)))))
(constraint
 (or (not (and (>= i19 0) (and (>= i18 0) (and (>= i18 i17) (>= i19 i17)))))
  (= (max (+ (max (+ i18 i13) 0) i8) 0)
   (odot$1
    (mk_synd_tup_Int_Int_Int (+ (+ 0 i13) i8) (max (+ (max (+ 0 i13) 0) i8) 0)
     (max (max 0 (+ 0 i13)) (+ (+ 0 i13) i8)))
    (mk_synd_tup_Int_Int_Int i17 i18 i19)))))
(check-synth)
