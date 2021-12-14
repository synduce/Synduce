(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun odot$1 ((x60 synd_tup_Int_Int_Int_Int) (x61 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x60) (proj_synd_tup_Int_Int_Int_Int_1 x60)
    (proj_synd_tup_Int_Int_Int_Int_2 x60) (proj_synd_tup_Int_Int_Int_Int_3 x60)
    (proj_synd_tup_Int_Int_Int_Int_0 x61) (proj_synd_tup_Int_Int_Int_Int_1 x61)
    (proj_synd_tup_Int_Int_Int_Int_2 x61) (proj_synd_tup_Int_Int_Int_Int_3 x61) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i108 Int)
(declare-var i83 Int)
(declare-var i48 Int)
(declare-var i17 Int)
(declare-var i11 Int)
(declare-var p4 Int)
(declare-var i116 Int)
(declare-var i115 Int)
(declare-var i114 Int)
(declare-var i113 Int)
(constraint
 (or
  (not
   (and (>= i114 0)
    (and (>= i115 0)
     (and (>= i115 i113)
      (and (>= i114 i113)
       (and (>= i116 0) (and (>= i116 i114) (and (>= i116 i113) (>= i116 i115)))))))))
  (= i114
   (odot$1 (mk_synd_tup_Int_Int_Int_Int 0 0 0 0) (mk_synd_tup_Int_Int_Int_Int i113 i114 i115 i116)))))
(constraint
 (or
  (not
   (and (>= i114 0)
    (and (>= i115 0)
     (and (>= i115 i113)
      (and (>= i114 i113)
       (and (>= i116 0) (and (>= i116 i114) (and (>= i116 i113) (>= i116 i115)))))))))
  (= (max i114 (+ i113 p4))
   (odot$1
    (mk_synd_tup_Int_Int_Int_Int (+ 0 p4) (max 0 (+ 0 p4)) (max (+ 0 p4) 0)
     (max 0 (max (+ 0 p4) 0)))
    (mk_synd_tup_Int_Int_Int_Int i113 i114 i115 i116)))))
(constraint
 (or
  (not
   (and (>= i114 0)
    (and (>= i115 0)
     (and (>= i115 i113)
      (and (>= i114 i113)
       (and (>= i116 0) (and (>= i116 i114) (and (>= i116 i113) (>= i116 i115)))))))))
  (= (max i114 (+ i113 i11))
   (odot$1
    (mk_synd_tup_Int_Int_Int_Int (+ 0 i11) (max 0 (+ 0 i11)) (max (+ 0 i11) 0)
     (max 0 (max (+ 0 i11) 0)))
    (mk_synd_tup_Int_Int_Int_Int i113 i114 i115 i116)))))
(constraint
 (or
  (not
   (and (>= i114 0)
    (and (>= i115 0)
     (and (>= i115 i113)
      (and (>= i114 i113)
       (and (>= i116 0) (and (>= i116 i114) (and (>= i116 i113) (>= i116 i115)))))))))
  (= (max (max i114 (+ i113 i17)) (+ (+ i113 i17) i11))
   (odot$1
    (mk_synd_tup_Int_Int_Int_Int (+ (+ 0 i17) i11) (max (max 0 (+ 0 i17)) (+ (+ 0 i17) i11))
     (max (+ (max (+ 0 i17) 0) i11) 0)
     (max (max 0 (max (+ 0 i17) 0)) (max (+ (max (+ 0 i17) 0) i11) 0)))
    (mk_synd_tup_Int_Int_Int_Int i113 i114 i115 i116)))))
(constraint
 (or
  (not
   (and (>= i114 0)
    (and (>= i115 0)
     (and (>= i115 i113)
      (and (>= i114 i113)
       (and (>= i116 0) (and (>= i116 i114) (and (>= i116 i113) (>= i116 i115)))))))))
  (= (max (max i114 (+ i113 i83)) (+ (+ i113 i83) i48))
   (odot$1
    (mk_synd_tup_Int_Int_Int_Int (+ (+ 0 i83) i48) (max (max 0 (+ 0 i83)) (+ (+ 0 i83) i48))
     (max (+ (max (+ 0 i83) 0) i48) 0)
     (max (max 0 (max (+ 0 i83) 0)) (max (+ (max (+ 0 i83) 0) i48) 0)))
    (mk_synd_tup_Int_Int_Int_Int i113 i114 i115 i116)))))
(constraint
 (or
  (not
   (and (>= i114 0)
    (and (>= i115 0)
     (and (>= i115 i113)
      (and (>= i114 i113)
       (and (>= i116 0) (and (>= i116 i114) (and (>= i116 i113) (>= i116 i115)))))))))
  (= (max (max (max i114 (+ i113 i108)) (+ (+ i113 i108) i83)) (+ (+ (+ i113 i108) i83) i48))
   (odot$1
    (mk_synd_tup_Int_Int_Int_Int (+ (+ (+ 0 i108) i83) i48)
     (max (max (max 0 (+ 0 i108)) (+ (+ 0 i108) i83)) (+ (+ (+ 0 i108) i83) i48))
     (max (+ (max (+ (max (+ 0 i108) 0) i83) 0) i48) 0)
     (max (max (max 0 (max (+ 0 i108) 0)) (max (+ (max (+ 0 i108) 0) i83) 0))
      (max (+ (max (+ (max (+ 0 i108) 0) i83) 0) i48) 0)))
    (mk_synd_tup_Int_Int_Int_Int i113 i114 i115 i116)))))
(check-synth)
