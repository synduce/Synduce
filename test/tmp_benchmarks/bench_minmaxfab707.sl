(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun join$0 ((x24 Int) (x25 synd_tup_Int_Int) (x26 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((min Ix Ix)))
  (Ix Int
   (Ic x24 (proj_synd_tup_Int_Int_0 x25) (proj_synd_tup_Int_Int_1 x25)
    (proj_synd_tup_Int_Int_0 x26) (proj_synd_tup_Int_Int_1 x26) (- Ix) 
    (+ Ix Ix) (min Ix Ix) (max Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i15 Int)
(declare-var i Int)
(declare-var p8 Int)
(declare-var p7 Int)
(declare-var p3 Int)
(declare-var p0 Int)
(constraint
 (= (min (min p0 p3) p7) (join$0 p0 (mk_synd_tup_Int_Int p3 p3) (mk_synd_tup_Int_Int p7 p7))))
(constraint
 (= (min (min (min (min p0 p3) p8) i) i15)
  (join$0 p0 (mk_synd_tup_Int_Int p3 p3)
   (mk_synd_tup_Int_Int (min (min p8 i) i15) (max (max p8 i) i15)))))
(check-synth)
