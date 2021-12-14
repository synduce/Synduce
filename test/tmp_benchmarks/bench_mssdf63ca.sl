(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun odot$3 ((x44 synd_tup_Int_Int_Int_Int) (x45 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x44) (proj_synd_tup_Int_Int_Int_Int_1 x44)
    (proj_synd_tup_Int_Int_Int_Int_2 x44) (proj_synd_tup_Int_Int_Int_Int_3 x44)
    (proj_synd_tup_Int_Int_Int_Int_0 x45) (proj_synd_tup_Int_Int_Int_Int_1 x45)
    (proj_synd_tup_Int_Int_Int_Int_2 x45) (proj_synd_tup_Int_Int_Int_Int_3 x45) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i18 Int)
(declare-var i16 Int)
(declare-var i30 Int)
(declare-var i29 Int)
(declare-var i28 Int)
(declare-var i27 Int)
(declare-var i2 Int)
(declare-var i Int)
(constraint
 (or
  (not
   (and
    (and (>= i28 0)
     (and (>= i29 0)
      (and (>= i29 i27)
       (and (>= i28 i27) (and (>= i30 0) (and (>= i30 i28) (and (>= i30 i27) (>= i30 i29))))))))
    (not (< i 0))))
  (= (max i30 (max (+ i29 i2) 0))
   (odot$3 (mk_synd_tup_Int_Int_Int_Int i2 (max i2 0) (max i2 0) (max i2 0))
    (mk_synd_tup_Int_Int_Int_Int i27 i28 i29 i30)))))
(constraint
 (or
  (not
   (and
    (and (>= i28 0)
     (and (>= i29 0)
      (and (>= i29 i27)
       (and (>= i28 i27) (and (>= i30 0) (and (>= i30 i28) (and (>= i30 i27) (>= i30 i29))))))))
    (not (< i 0))))
  (= (max (max i30 (max (+ i29 i18) 0)) (max (+ (max (+ i29 i18) 0) i16) 0))
   (odot$3
    (mk_synd_tup_Int_Int_Int_Int (+ i18 i16) (max (max i18 0) (+ i18 i16))
     (max (+ (max i18 0) i16) 0) (max (max i18 0) (max (+ (max i18 0) i16) 0)))
    (mk_synd_tup_Int_Int_Int_Int i27 i28 i29 i30)))))
(check-synth)
