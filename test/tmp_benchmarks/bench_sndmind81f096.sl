(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun odot$1 ((x43 synd_tup_Int_Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((min Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x43) (proj_synd_tup_Int_Int_1 x43) (- Ix) 
    (+ Ix Ix) (min Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i30612 Int)
(declare-var i30611 Int)
(declare-var p6 Int)
(declare-var p5 Int)
(constraint
 (or
  (not
   (and
    (and (and (> p5 p6) (and (> p6 i30612) (> i30612 i30611)))
     (and (> p5 p6) (and (> p5 p6) (> p6 i30612))))
    (and (> p5 p6) (and (> p5 p6) (> p5 i30612)))))
  (= (min (min i30612 (max p6 i30611)) (max p5 (min p6 i30611)))
   (odot$1 (mk_synd_tup_Int_Int i30611 i30612)))))
(check-synth)
