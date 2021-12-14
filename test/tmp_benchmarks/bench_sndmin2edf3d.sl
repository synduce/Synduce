(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun odot$1 ((x4 synd_tup_Int_Int) (x5 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((min Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x4) (proj_synd_tup_Int_Int_1 x4) (proj_synd_tup_Int_Int_0 x5)
    (proj_synd_tup_Int_Int_1 x5) (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i0 Int)
(declare-var i Int)
(declare-var p6 Int)
(declare-var p5 Int)
(constraint
 (= (min (min i0 (max p6 i)) (max p5 (min p6 i)))
  (odot$1 (mk_synd_tup_Int_Int (min p5 p6) (max p5 p6)) (mk_synd_tup_Int_Int i i0))))
(check-synth)
