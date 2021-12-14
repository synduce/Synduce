(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join$0 ((x33 synd_tup_Int_Int) (x34 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x33) (proj_synd_tup_Int_Int_1 x33) (proj_synd_tup_Int_Int_0 x34)
    (proj_synd_tup_Int_Int_1 x34) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i52 Int)
(declare-var i48 Int)
(declare-var i Int)
(declare-var i56 Int)
(declare-var i55 Int)
(constraint
 (or (not (>= i55 i56)) (= i55 (join$0 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int i55 i56)))))
(constraint
 (or (not (>= i55 i56))
  (= (max i55 (+ i56 i))
   (join$0 (mk_synd_tup_Int_Int (max 0 (+ 0 i)) (+ 0 i)) (mk_synd_tup_Int_Int i55 i56)))))
(constraint
 (or (not (>= i55 i56))
  (= (max i55 (+ i56 i48))
   (join$0 (mk_synd_tup_Int_Int (max 0 (+ 0 i48)) (+ 0 i48)) (mk_synd_tup_Int_Int i55 i56)))))
(constraint
 (or (not (>= i55 i56))
  (= (max (max i55 (+ i56 i52)) (+ (+ i56 i52) i48))
   (join$0 (mk_synd_tup_Int_Int (max (max 0 (+ 0 i52)) (+ (+ 0 i52) i48)) (+ (+ 0 i52) i48))
    (mk_synd_tup_Int_Int i55 i56)))))
(check-synth)
