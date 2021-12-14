(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join1$0 ((x59 Int) (x60 synd_tup_Int_Int) (x61 synd_tup_Int_Int)) Int
 ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic x59 (proj_synd_tup_Int_Int_0 x60) (proj_synd_tup_Int_Int_1 x60)
    (proj_synd_tup_Int_Int_0 x61) (proj_synd_tup_Int_Int_1 x61) (- Ix) 
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
(constraint (= p0 (join1$0 p0 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int 0 0))))
(constraint
 (= (+ p0 p7)
  (join1$0 p0 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int (+ 0 p7) (max 0 (+ 0 p7))))))
(constraint
 (= (+ p0 p3)
  (join1$0 p0 (mk_synd_tup_Int_Int (+ 0 p3) (max 0 (+ 0 p3))) (mk_synd_tup_Int_Int 0 0))))
(constraint
 (= (+ (+ p0 p3) i)
  (join1$0 p0 (mk_synd_tup_Int_Int (+ 0 p3) (max 0 (+ 0 p3)))
   (mk_synd_tup_Int_Int (+ 0 i) (max 0 (+ 0 i))))))
(constraint
 (= (+ (+ p0 p4) i7)
  (join1$0 p0 (mk_synd_tup_Int_Int (+ (+ 0 p4) i7) (max (max 0 (+ 0 p4)) (+ (+ 0 p4) i7)))
   (mk_synd_tup_Int_Int 0 0))))
(constraint
 (= (+ (+ (+ p0 p4) i7) i21)
  (join1$0 p0 (mk_synd_tup_Int_Int (+ (+ 0 p4) i7) (max (max 0 (+ 0 p4)) (+ (+ 0 p4) i7)))
   (mk_synd_tup_Int_Int (+ 0 i21) (max 0 (+ 0 i21))))))
(constraint
 (= (+ (+ (+ p0 p3) i0) i11)
  (join1$0 p0 (mk_synd_tup_Int_Int (+ 0 p3) (max 0 (+ 0 p3)))
   (mk_synd_tup_Int_Int (+ (+ 0 i0) i11) (max (max 0 (+ 0 i0)) (+ (+ 0 i0) i11))))))
(constraint
 (= (+ (+ (+ (+ p0 p3) i0) i11) i29)
  (join1$0 p0 (mk_synd_tup_Int_Int (+ 0 p3) (max 0 (+ 0 p3)))
   (mk_synd_tup_Int_Int (+ (+ (+ 0 i0) i11) i29)
    (max (max (max 0 (+ 0 i0)) (+ (+ 0 i0) i11)) (+ (+ (+ 0 i0) i11) i29))))))
(check-synth)
