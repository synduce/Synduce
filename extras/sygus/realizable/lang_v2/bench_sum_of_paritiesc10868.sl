(set-logic DTNIA)
(synth-fun c1 () Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic (- Ix) (+ Ix Ix))) (Ic Int ((Constant Int)))))
(declare-var p Int)
(declare-var i79 Int)
(declare-var i80 Int)
(constraint
 (or
  (not
   (and (and (>= i79 0) (>= i80 0))
    (and
     (and
      (or
       (not
        (and (and (>= i79 0) (>= i80 0))
         (and
          (or
           (not (and (and (>= i79 0) (>= i80 0)) (or (not (and (>= i79 0) (>= i80 0))) (= i79 0))))
           (= i79 i80))
          (or (not (and (>= i79 0) (>= i80 0))) (= i79 0)))))
       (= i79 (mod (+ p i80) 2)))
      (or (not (and (and (>= i79 0) (>= i80 0)) (or (not (and (>= i79 0) (>= i80 0))) (= i79 0))))
       (= i79 i80)))
     (or (not (and (>= i79 0) (>= i80 0))) (= i79 0)))))
  (= (+ (+ (mod p 2) i79) i80) c1)))
(check-synth)
