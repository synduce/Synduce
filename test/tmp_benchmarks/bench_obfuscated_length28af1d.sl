(set-logic LIA)
(synth-fun join ((x4 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x4 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i4 Int)
(declare-var i1 Int)
(declare-var i2 Int)
(declare-var i Int)
(declare-var i0 Int)
(declare-var p Int)
(constraint (or (not (>= (+ 1 1) 2)) (= (ite (< p i0) 2 2) (join 1))))
(constraint
 (or (not (>= (+ 1 (+ 1 1)) 2))
  (=
   (ite (< i i2) (ite (< p i) 3 (+ 1 (ite (< p i2) 2 2))) (ite (< p i2) 3 (+ 1 (ite (< p i) 2 2))))
   (join (ite (< i i2) (+ 1 1) (+ 1 1))))))
(constraint
 (or (not (>= (+ 1 (+ 1 (+ 1 1))) 2))
  (=
   (ite (< i1 i4)
    (ite (< i i1) (ite (< p i) 4 (+ 1 (ite (< p i1) 3 (+ 1 (ite (< p i4) 2 2)))))
     (ite (< p i1) (+ 1 (+ 1 (ite (< i i4) 2 2)))
      (+ 1
       (ite (< i i4) (ite (< p i) 3 (+ 1 (ite (< p i4) 2 2)))
        (ite (< p i4) 3 (+ 1 (ite (< p i) 2 2)))))))
    (ite (< i i4) (ite (< p i) 4 (+ 1 (ite (< p i4) 3 (+ 1 (ite (< p i1) 2 2)))))
     (ite (< p i4) (+ 1 (+ 1 (ite (< i i1) 2 2)))
      (+ 1
       (ite (< i i1) (ite (< p i) 3 (+ 1 (ite (< p i1) 2 2)))
        (ite (< p i1) 3 (+ 1 (ite (< p i) 2 2))))))))
   (join
    (ite (< i1 i4) (ite (< i i1) (+ 1 (+ 1 1)) (+ 1 (ite (< i i4) (+ 1 1) (+ 1 1))))
     (ite (< i i4) (+ 1 (+ 1 1)) (+ 1 (ite (< i i1) (+ 1 1) (+ 1 1)))))))))
(check-synth)
