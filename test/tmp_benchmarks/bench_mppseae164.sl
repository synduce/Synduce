(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join1$1 ((x53 Int) (x54 synd_tup_Int_Int) (x55 synd_tup_Int_Int)) Int
 ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic x53 (proj_synd_tup_Int_Int_0 x54) (proj_synd_tup_Int_Int_1 x54)
    (proj_synd_tup_Int_Int_0 x55) (proj_synd_tup_Int_Int_1 x55) (- Ix) 
    (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i29 Int)
(declare-var i11 Int)
(declare-var i0 Int)
(declare-var i21 Int)
(declare-var i7 Int)
(declare-var p4 Int)
(declare-var i Int)
(declare-var p3 Int)
(declare-var p7 Int)
(declare-var p0 Int)
(constraint (= (max 0 p0) (join1$1 p0 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int 0 0))))
(constraint
 (= (max (max 0 p0) (+ p0 p7))
  (join1$1 p0 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int (+ 0 p7) (max 0 (+ 0 p7))))))
(constraint
 (= (max (max 0 p0) (+ p0 p3))
  (join1$1 p0 (mk_synd_tup_Int_Int (+ 0 p3) (max 0 (+ 0 p3))) (mk_synd_tup_Int_Int 0 0))))
(constraint
 (= (max (max (max 0 p0) (+ p0 p3)) (+ (+ p0 p3) i))
  (join1$1 p0 (mk_synd_tup_Int_Int (+ 0 p3) (max 0 (+ 0 p3)))
   (mk_synd_tup_Int_Int (+ 0 i) (max 0 (+ 0 i))))))
(constraint
 (= (max (max (max 0 p0) (+ p0 p4)) (+ (+ p0 p4) i7))
  (join1$1 p0 (mk_synd_tup_Int_Int (+ (+ 0 p4) i7) (max (max 0 (+ 0 p4)) (+ (+ 0 p4) i7)))
   (mk_synd_tup_Int_Int 0 0))))
(constraint
 (= (max (max (max (max 0 p0) (+ p0 p4)) (+ (+ p0 p4) i7)) (+ (+ (+ p0 p4) i7) i21))
  (join1$1 p0 (mk_synd_tup_Int_Int (+ (+ 0 p4) i7) (max (max 0 (+ 0 p4)) (+ (+ 0 p4) i7)))
   (mk_synd_tup_Int_Int (+ 0 i21) (max 0 (+ 0 i21))))))
(constraint
 (= (max (max (max (max 0 p0) (+ p0 p3)) (+ (+ p0 p3) i0)) (+ (+ (+ p0 p3) i0) i11))
  (join1$1 p0 (mk_synd_tup_Int_Int (+ 0 p3) (max 0 (+ 0 p3)))
   (mk_synd_tup_Int_Int (+ (+ 0 i0) i11) (max (max 0 (+ 0 i0)) (+ (+ 0 i0) i11))))))
(constraint
 (=
  (max (max (max (max (max 0 p0) (+ p0 p3)) (+ (+ p0 p3) i0)) (+ (+ (+ p0 p3) i0) i11))
   (+ (+ (+ (+ p0 p3) i0) i11) i29))
  (join1$1 p0 (mk_synd_tup_Int_Int (+ 0 p3) (max 0 (+ 0 p3)))
   (mk_synd_tup_Int_Int (+ (+ (+ 0 i0) i11) i29)
    (max (max (max 0 (+ 0 i0)) (+ (+ 0 i0) i11)) (+ (+ (+ 0 i0) i11) i29))))))
(check-synth)
