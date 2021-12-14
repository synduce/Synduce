(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun s1$0 ((x35 synd_tup_Int_Int) (x36 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x35) (proj_synd_tup_Int_Int_1 x35) (proj_synd_tup_Int_Int_0 x36)
    (proj_synd_tup_Int_Int_1 x36) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i20 Int)
(declare-var i10 Int)
(declare-var i472 Int)
(declare-var i471 Int)
(declare-var i2 Int)
(constraint
 (or (not (>= i471 i472))
  (= (max (+ i472 i2) i471)
   (s1$0 (mk_synd_tup_Int_Int i471 i472) (mk_synd_tup_Int_Int (max 0 i2) i2)))))
(constraint
 (or (not (>= i471 i472))
  (= (max (+ (+ i472 i20) i10) (max (+ i472 i20) i471))
   (s1$0 (mk_synd_tup_Int_Int i471 i472)
    (mk_synd_tup_Int_Int (max (+ i20 i10) (max 0 i20)) (+ i20 i10))))))
(check-synth)
