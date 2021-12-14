(set-logic LIA)
(synth-fun c0 () Bool)
(declare-var i14211 Int)
(declare-var b15 Bool)
(declare-var i14210 Int)
(declare-var b14 Bool)
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
          (and (or (not (not (> i0 2))) (= i1 (+ i14210 i14211)))
           (or (not (not (> i0 2))) (= i1 (+ i14210 i14211))))
          (or (not (not (> i0 2))) (= i1 (+ i14210 i14211))))
         (or (not (not (> i0 2))) (= i14210 1)))
        (or (not (not (> i0 2))) (= i0 2)))
       (or (not (not (> i0 2))) (> i14211 i0)))
      (or (not (not (> i0 2))) b14))
     (and (or (not (> i0 2)) (= i1 (+ i14210 i14211))) (or (not (> i0 2)) (> i14211 i0))))
    (> i0 2)))
  (= (and b14 (and b15 (<= (+ (+ i14210 i14211) i0) 2))) c0)))
(check-synth)
