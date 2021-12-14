(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun s1$0 ((x37 synd_tup_Int_Int) (x38 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x37) (proj_synd_tup_Int_Int_1 x37) (proj_synd_tup_Int_Int_0 x38)
    (proj_synd_tup_Int_Int_1 x38) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i20 Int)
(declare-var i10 Int)
(declare-var i1894 Int)
(declare-var i1893 Int)
(declare-var i2 Int)
(constraint
 (or (not (>= i1893 i1894))
  (= (max (+ i2 i1893) 0)
   (s1$0 (mk_synd_tup_Int_Int i1893 i1894) (mk_synd_tup_Int_Int (max 0 i2) i2)))))
(constraint
 (or (not (>= i1893 i1894))
  (= (max (+ i10 (max (+ i20 i1893) 0)) 0)
   (s1$0 (mk_synd_tup_Int_Int i1893 i1894)
    (mk_synd_tup_Int_Int (max (+ i10 (max 0 i20)) 0) (+ i20 i10))))))
(check-synth)
