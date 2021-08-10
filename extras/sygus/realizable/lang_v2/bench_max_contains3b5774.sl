(set-logic DTNIA)
(synth-fun c0 ((x129 Int) (x130 Int)) Bool ((Ipred Bool) (Ix Int) (Ic Int))
 ((Ipred Bool ((not Ipred) (and Ipred Ipred) (or Ipred Ipred) (= Ix Ic) (= Ix Ix) (> Ix Ix)))
  (Ix Int (Ic x129 x130 (- Ix) (+ Ix Ix) (+ Ix Ic) (ite Ipred Ix Ix))) 
  (Ic Int ((Constant Int)))))
(declare-var x Int)
(declare-var i Int)
(declare-var p Int)
(declare-var b7 Bool)
(declare-var b8 Bool)
(constraint
 (or
  (not
   (and
    (and
     (and
      (or
       (not
        (and
         (and (or (not (and (or (not (> x i)) (> x p)) (> x i))) (> x (ite b7 x p)))
          (or (not (> x i)) (> x p)))
         (> x i)))
       (> x (ite b7 x (ite b8 x p))))
      (or (not (and (or (not (> x i)) (> x p)) (> x i))) (> x (ite b7 x p))))
     (or (not (> x i)) (> x p)))
    (> x i)))
  (= (or (= x p) (or b7 b8)) (c0 x p))))
(check-synth)
