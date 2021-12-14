(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join1$1 ((x18 Int) (x19 synd_tup_Int_Int) (x20 synd_tup_Int_Int)) Int
 ((Ix Int) (Ic Int))
 ((Ix Int
   (Ic x18 (proj_synd_tup_Int_Int_0 x19) (proj_synd_tup_Int_Int_1 x19)
    (proj_synd_tup_Int_Int_0 x20) (proj_synd_tup_Int_Int_1 x20) (- Ix) 
    (+ Ix Ix) (max Ix Ix)))
  (Ic Int ((Constant Int)))))
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
(check-synth)
