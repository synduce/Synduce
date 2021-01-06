(set-logic DTLIA)
(synth-fun join0 ((x Int) (x59 (Tuple Bool Int)) (x60 (Tuple Bool Int))) Bool
 ((Ipred Bool) (Ix Int) (Ic Int))
 ((Ipred Bool
   (((_ tupSel 0) x59) ((_ tupSel 0) x60) (not Ipred) (and Ipred Ipred) (ite Ipred Ipred Ipred)
    (or Ipred Ipred) (= Ix Ix) (> Ix Ix)))
  (Ix Int (Ic x ((_ tupSel 1) x59) ((_ tupSel 1) x60) (mod Ix 2) (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int (0 1))))

(declare-var new31 Int)
(declare-var new37 Int)
(declare-var b_ Bool)
(declare-var i_ Int)
(declare-var b_53 Bool)
(declare-var i_55 Int)


(constraint
 (and
  (or (or b_ (or (= (mod new37 2) 0) b_53)) (= (mod new31 2) 0))

  (join0 new31
   (mkTuple (or b_ (or (= (mod new37 2) 0) b_53))
    (ite b_ i_ (ite (= (mod new37 2) 0) new37 i_55)))
   (mkTuple false 1))))

(constraint
 (not
  (join0 new31
   (mkTuple
    (or b_ (or (= (mod new37 2) 0) b_53))
    (ite b_ i_ (ite (= (mod new37 2) 0) new37 i_55)))
   (mkTuple false 1))))

(check-synth)
