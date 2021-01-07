(set-logic DTNIA)
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun odot0 ((x66 (Tuple Int Int Bool)) (x67 (Tuple Int Int Bool))) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int
    ((min Ix (+ Ix Ix))))
   (Ix Int
   (Ic ((_ tupSel 0) x66) ((_ tupSel 1) x66) ((_ tupSel 0) x67) ((_ tupSel 1) x67)
    (- Ix) (+ Ix Ix) (min Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int (0 1))
  (Ipred Bool
   (((_ tupSel 2) x66) ((_ tupSel 2) x67) (= Ix Ix) (> Ix Ix) (not Ipred)
    (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var new47 Bool)
(declare-var i_ Int)
(declare-var i_51 Int)
(declare-var b_ Bool)
(constraint (= i_ (odot0 (mkTuple 0 0 true) (mkTuple i_ i_51 b_))))
(constraint
 (= (min i_ (ite new47 (+ i_51 1) (- i_51 1)))
  (odot0
   (mkTuple (min 0 (ite new47 (+ 0 1) (- 0 1))) (ite new47 (+ 0 1) (- 0 1))
    (and true (>= (ite new47 (+ 0 1) (- 0 1)) 0)))
   (mkTuple i_ i_51 b_))))
(check-synth)
