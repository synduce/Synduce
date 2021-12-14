(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun odot$1 ((x27 Int) (x28 synd_tup_Int_Int) (x29 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic x27 (proj_synd_tup_Int_Int_0 x28) (proj_synd_tup_Int_Int_1 x28)
    (proj_synd_tup_Int_Int_0 x29) (proj_synd_tup_Int_Int_1 x29) (- Ix) 
    (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i4 Int)
(declare-var p2 Int)
(declare-var i18 Int)
(declare-var i17 Int)
(declare-var p Int)
(constraint
 (or (not (and (> i18 i17) (> i18 0)))
  (= (max (+ i17 p) i18) (odot$1 p (mk_synd_tup_Int_Int i17 i18) (mk_synd_tup_Int_Int 0 0)))))
(constraint
 (or (not (and (> i18 i17) (> i18 0)))
  (= (max (+ (+ i17 p2) p) (max (+ i17 p2) i18))
   (odot$1 p (mk_synd_tup_Int_Int i17 i18) (mk_synd_tup_Int_Int (+ 0 p2) (max (+ 0 p2) 0))))))
(constraint
 (or (not (and (> i18 i17) (> i18 0)))
  (= (max (+ (+ (+ i17 i4) p2) p) (max (+ (+ i17 i4) p2) (max (+ i17 i4) i18)))
   (odot$1 p (mk_synd_tup_Int_Int i17 i18)
    (mk_synd_tup_Int_Int (+ (+ 0 i4) p2) (max (+ (+ 0 i4) p2) (max (+ 0 i4) 0)))))))
(check-synth)
