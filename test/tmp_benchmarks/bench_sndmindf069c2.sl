(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun odot$1 ((x31 synd_tup_Int_Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((min Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x31) (proj_synd_tup_Int_Int_1 x31) (- Ix) 
    (+ Ix Ix) (min Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i4634 Int)
(declare-var i4633 Int)
(declare-var p6 Int)
(declare-var p5 Int)
(constraint
 (or
  (not
   (and (and (> p5 p6) (and (> p5 p6) (> p6 i4634))) (and (> p5 p6) (and (> p5 p6) (> p5 i4634)))))
  (= (min (min i4634 (max p6 i4633)) (max p5 (min p6 i4633)))
   (odot$1 (mk_synd_tup_Int_Int i4633 i4634)))))
(check-synth)
