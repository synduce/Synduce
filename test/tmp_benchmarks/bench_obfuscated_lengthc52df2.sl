(set-logic LIA)
(synth-fun join ((x7 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x7 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i6 Int)
(declare-var i3 Int)
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
(constraint
 (or (not (>= (+ 1 (+ 1 (+ 1 (+ 1 1)))) 2))
  (=
   (ite (< i3 i6)
    (ite (< i1 i3)
     (ite (< i i1)
      (ite (< p i) 5 (+ 1 (ite (< p i1) 4 (+ 1 (ite (< p i3) 3 (+ 1 (ite (< p i6) 2 2)))))))
      (ite (< p i1) (+ 1 (+ 1 (ite (< i i3) 3 (+ 1 (ite (< i i6) 2 2)))))
       (+ 1
        (ite (< i i3) (ite (< p i) 4 (+ 1 (ite (< p i3) 3 (+ 1 (ite (< p i6) 2 2)))))
         (ite (< p i3) (+ 1 (+ 1 (ite (< i i6) 2 2)))
          (+ 1
           (ite (< i i6) (ite (< p i) 3 (+ 1 (ite (< p i6) 2 2)))
            (ite (< p i6) 3 (+ 1 (ite (< p i) 2 2))))))))))
     (ite (< i i3)
      (ite (< p i) (+ 1 (+ 1 (+ 1 (ite (< i1 i6) 2 2))))
       (+ 1
        (ite (< p i3) (+ 1 (+ 1 (ite (< i1 i6) 2 2)))
         (+ 1
          (ite (< i1 i6) (ite (< p i1) 3 (+ 1 (ite (< p i6) 2 2)))
           (ite (< p i6) 3 (+ 1 (ite (< p i1) 2 2))))))))
      (ite (< p i3)
       (+ 1
        (+ 1
         (ite (< i1 i6) (ite (< i i1) 3 (+ 1 (ite (< i i6) 2 2)))
          (ite (< i i6) 3 (+ 1 (ite (< i i1) 2 2))))))
       (+ 1
        (ite (< i1 i6)
         (ite (< i i1) (ite (< p i) 4 (+ 1 (ite (< p i1) 3 (+ 1 (ite (< p i6) 2 2)))))
          (ite (< p i1) (+ 1 (+ 1 (ite (< i i6) 2 2)))
           (+ 1
            (ite (< i i6) (ite (< p i) 3 (+ 1 (ite (< p i6) 2 2)))
             (ite (< p i6) 3 (+ 1 (ite (< p i) 2 2)))))))
         (ite (< i i6) (ite (< p i) 4 (+ 1 (ite (< p i6) 3 (+ 1 (ite (< p i1) 2 2)))))
          (ite (< p i6) (+ 1 (+ 1 (ite (< i i1) 2 2)))
           (+ 1
            (ite (< i i1) (ite (< p i) 3 (+ 1 (ite (< p i1) 2 2)))
             (ite (< p i1) 3 (+ 1 (ite (< p i) 2 2))))))))))))
    (ite (< i1 i6)
     (ite (< i i1)
      (ite (< p i) 5 (+ 1 (ite (< p i1) 4 (+ 1 (ite (< p i6) 3 (+ 1 (ite (< p i3) 2 2)))))))
      (ite (< p i1) (+ 1 (+ 1 (ite (< i i6) 3 (+ 1 (ite (< i i3) 2 2)))))
       (+ 1
        (ite (< i i6) (ite (< p i) 4 (+ 1 (ite (< p i6) 3 (+ 1 (ite (< p i3) 2 2)))))
         (ite (< p i6) (+ 1 (+ 1 (ite (< i i3) 2 2)))
          (+ 1
           (ite (< i i3) (ite (< p i) 3 (+ 1 (ite (< p i3) 2 2)))
            (ite (< p i3) 3 (+ 1 (ite (< p i) 2 2))))))))))
     (ite (< i i6)
      (ite (< p i) (+ 1 (+ 1 (+ 1 (ite (< i1 i3) 2 2))))
       (+ 1
        (ite (< p i6) (+ 1 (+ 1 (ite (< i1 i3) 2 2)))
         (+ 1
          (ite (< i1 i3) (ite (< p i1) 3 (+ 1 (ite (< p i3) 2 2)))
           (ite (< p i3) 3 (+ 1 (ite (< p i1) 2 2))))))))
      (ite (< p i6)
       (+ 1
        (+ 1
         (ite (< i1 i3) (ite (< i i1) 3 (+ 1 (ite (< i i3) 2 2)))
          (ite (< i i3) 3 (+ 1 (ite (< i i1) 2 2))))))
       (+ 1
        (ite (< i1 i3)
         (ite (< i i1) (ite (< p i) 4 (+ 1 (ite (< p i1) 3 (+ 1 (ite (< p i3) 2 2)))))
          (ite (< p i1) (+ 1 (+ 1 (ite (< i i3) 2 2)))
           (+ 1
            (ite (< i i3) (ite (< p i) 3 (+ 1 (ite (< p i3) 2 2)))
             (ite (< p i3) 3 (+ 1 (ite (< p i) 2 2)))))))
         (ite (< i i3) (ite (< p i) 4 (+ 1 (ite (< p i3) 3 (+ 1 (ite (< p i1) 2 2)))))
          (ite (< p i3) (+ 1 (+ 1 (ite (< i i1) 2 2)))
           (+ 1
            (ite (< i i1) (ite (< p i) 3 (+ 1 (ite (< p i1) 2 2)))
             (ite (< p i1) 3 (+ 1 (ite (< p i) 2 2)))))))))))))
   (join
    (ite (< i3 i6)
     (ite (< i1 i3)
      (ite (< i i1) (+ 1 (+ 1 (+ 1 1)))
       (+ 1 (ite (< i i3) (+ 1 (+ 1 1)) (+ 1 (ite (< i i6) (+ 1 1) (+ 1 1))))))
      (ite (< i i3) (+ 1 (+ 1 (ite (< i1 i6) (+ 1 1) (+ 1 1))))
       (+ 1
        (ite (< i1 i6) (ite (< i i1) (+ 1 (+ 1 1)) (+ 1 (ite (< i i6) (+ 1 1) (+ 1 1))))
         (ite (< i i6) (+ 1 (+ 1 1)) (+ 1 (ite (< i i1) (+ 1 1) (+ 1 1))))))))
     (ite (< i1 i6)
      (ite (< i i1) (+ 1 (+ 1 (+ 1 1)))
       (+ 1 (ite (< i i6) (+ 1 (+ 1 1)) (+ 1 (ite (< i i3) (+ 1 1) (+ 1 1))))))
      (ite (< i i6) (+ 1 (+ 1 (ite (< i1 i3) (+ 1 1) (+ 1 1))))
       (+ 1
        (ite (< i1 i3) (ite (< i i1) (+ 1 (+ 1 1)) (+ 1 (ite (< i i3) (+ 1 1) (+ 1 1))))
         (ite (< i i3) (+ 1 (+ 1 1)) (+ 1 (ite (< i i1) (+ 1 1) (+ 1 1)))))))))))))
(check-synth)
