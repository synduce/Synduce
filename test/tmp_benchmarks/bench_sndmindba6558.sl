(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun odot$0 ((x46 synd_tup_Int_Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((min Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x46) (proj_synd_tup_Int_Int_1 x46) (- Ix) 
    (+ Ix Ix) (min Ix Ix) (ite Ipred Ix Ix)))
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
  (= (min p5 (min p6 i30611)) (odot$0 (mk_synd_tup_Int_Int i30611 i30612)))))
(check-synth)
