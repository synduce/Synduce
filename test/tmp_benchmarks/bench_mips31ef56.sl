(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun joinl$0 ((x36 Int) (x37 synd_tup_Int_Int) (x38 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix)))
  (Ix Int
   (Ic x36 (proj_synd_tup_Int_Int_0 x37) (proj_synd_tup_Int_Int_1 x37)
    (proj_synd_tup_Int_Int_0 x38) (proj_synd_tup_Int_Int_1 x38) (- Ix) 
    (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i494 Int)
(declare-var i505 Int)
(declare-var i504 Int)
(declare-var i2 Int)
(declare-var i4 Int)
(declare-var i501 Int)
(declare-var i500 Int)
(declare-var i Int)
(constraint (= i (joinl$0 i (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int 0 0))))
(constraint
 (or (not (and (>= i501 0) (>= i501 i500)))
  (= (+ i500 i) (joinl$0 i (mk_synd_tup_Int_Int i500 i501) (mk_synd_tup_Int_Int 0 0)))))
(constraint
 (= (+ i i4) (joinl$0 i (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int (+ 0 i4) (max (+ 0 i4) 0)))))
(constraint
 (= (+ i i4) (joinl$0 i (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int (+ 0 i4) (max (+ 0 i4) 0)))))
(constraint
 (or (not (and (>= i505 0) (>= i505 i504)))
  (= (+ (+ i504 i2) i)
   (joinl$0 i (mk_synd_tup_Int_Int (+ i504 i2) (max (+ i504 i2) i505)) (mk_synd_tup_Int_Int 0 0)))))
(constraint
 (= (+ (+ i i494) i4)
  (joinl$0 i (mk_synd_tup_Int_Int 0 0)
   (mk_synd_tup_Int_Int (+ (+ 0 i494) i4) (max (+ (+ 0 i494) i4) (max (+ 0 i494) 0))))))
(constraint
 (= (+ (+ i i4) i494)
  (joinl$0 i (mk_synd_tup_Int_Int 0 0)
   (mk_synd_tup_Int_Int (+ (+ 0 i4) i494) (max (+ (+ 0 i4) i494) (max (+ 0 i4) 0))))))
(check-synth)
