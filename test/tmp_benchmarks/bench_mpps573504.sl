(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join1$0 ((x40 Int) (x41 synd_tup_Int_Int) (x42 synd_tup_Int_Int)) Int
 ((Ix Int) (Ic Int))
 ((Ix Int
   (Ic x40 (proj_synd_tup_Int_Int_0 x41) (proj_synd_tup_Int_Int_1 x41)
    (proj_synd_tup_Int_Int_0 x42) (proj_synd_tup_Int_Int_1 x42) (- Ix) 
    (+ Ix Ix) (max Ix Ix)))
  (Ic Int ((Constant Int)))))
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
(check-synth)
