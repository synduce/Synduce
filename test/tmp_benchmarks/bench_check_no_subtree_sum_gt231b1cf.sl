(set-logic LIA)
(synth-fun c0 () Bool)
(declare-var i17111 Int)
(declare-var b17 Bool)
(declare-var i17110 Int)
(declare-var b16 Bool)
(declare-var i1 Int)
(declare-var i0 Int)
(constraint
 (or
  (not
   (and
    (and
     (and
      (and
       (and
        (and
         (and
          (and
           (and (or (not (not (> i0 2))) (= i1 (+ i17110 i17111)))
            (or (not (not (> i0 2))) (= i1 (+ i17110 i17111))))
           (or (not (not (> i0 2))) (= i1 (+ i17110 i17111))))
          (or (not (not (> i0 2))) (= i1 (+ i17110 i17111))))
         (or (not (not (> i0 2))) (= i17110 1)))
        (or (not (not (> i0 2))) (= i0 2)))
       (or (not (not (> i0 2))) (> i17111 i0)))
      (or (not (not (> i0 2))) b16))
     (and (and (or (not (> i0 2)) (> i17110 0)) (or (not (> i0 2)) (= i1 (+ i17110 i17111))))
      (or (not (> i0 2)) (> i17111 i0))))
    (> i0 2)))
  (= (and b16 (and b17 (<= (+ (+ i17110 i17111) i0) 2))) c0)))
(check-synth)
