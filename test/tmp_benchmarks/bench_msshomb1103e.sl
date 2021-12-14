(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun odot$1 ((x36 synd_tup_Int_Int_Int_Int) (x37 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x36) (proj_synd_tup_Int_Int_Int_Int_1 x36)
    (proj_synd_tup_Int_Int_Int_Int_2 x36) (proj_synd_tup_Int_Int_Int_Int_3 x36)
    (proj_synd_tup_Int_Int_Int_Int_0 x37) (proj_synd_tup_Int_Int_Int_Int_1 x37)
    (proj_synd_tup_Int_Int_Int_Int_2 x37) (proj_synd_tup_Int_Int_Int_Int_3 x37) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i17 Int)
(declare-var i11 Int)
(declare-var p4 Int)
(declare-var i25 Int)
(declare-var i24 Int)
(declare-var i23 Int)
(declare-var i22 Int)
(constraint
 (or
  (not
   (and (>= i23 0)
    (and (>= i24 0)
     (and (>= i24 i22)
      (and (>= i23 i22) (and (>= i25 0) (and (>= i25 i23) (and (>= i25 i22) (>= i25 i24)))))))))
  (= i23
   (odot$1 (mk_synd_tup_Int_Int_Int_Int 0 0 0 0) (mk_synd_tup_Int_Int_Int_Int i22 i23 i24 i25)))))
(constraint
 (or
  (not
   (and (>= i23 0)
    (and (>= i24 0)
     (and (>= i24 i22)
      (and (>= i23 i22) (and (>= i25 0) (and (>= i25 i23) (and (>= i25 i22) (>= i25 i24)))))))))
  (= (max i23 (+ i22 p4))
   (odot$1
    (mk_synd_tup_Int_Int_Int_Int (+ 0 p4) (max 0 (+ 0 p4)) (max (+ 0 p4) 0)
     (max 0 (max (+ 0 p4) 0)))
    (mk_synd_tup_Int_Int_Int_Int i22 i23 i24 i25)))))
(constraint
 (or
  (not
   (and (>= i23 0)
    (and (>= i24 0)
     (and (>= i24 i22)
      (and (>= i23 i22) (and (>= i25 0) (and (>= i25 i23) (and (>= i25 i22) (>= i25 i24)))))))))
  (= (max i23 (+ i22 i11))
   (odot$1
    (mk_synd_tup_Int_Int_Int_Int (+ 0 i11) (max 0 (+ 0 i11)) (max (+ 0 i11) 0)
     (max 0 (max (+ 0 i11) 0)))
    (mk_synd_tup_Int_Int_Int_Int i22 i23 i24 i25)))))
(constraint
 (or
  (not
   (and (>= i23 0)
    (and (>= i24 0)
     (and (>= i24 i22)
      (and (>= i23 i22) (and (>= i25 0) (and (>= i25 i23) (and (>= i25 i22) (>= i25 i24)))))))))
  (= (max (max i23 (+ i22 i17)) (+ (+ i22 i17) i11))
   (odot$1
    (mk_synd_tup_Int_Int_Int_Int (+ (+ 0 i17) i11) (max (max 0 (+ 0 i17)) (+ (+ 0 i17) i11))
     (max (+ (max (+ 0 i17) 0) i11) 0)
     (max (max 0 (max (+ 0 i17) 0)) (max (+ (max (+ 0 i17) 0) i11) 0)))
    (mk_synd_tup_Int_Int_Int_Int i22 i23 i24 i25)))))
(check-synth)
