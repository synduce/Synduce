(set-logic NIA)
(synth-fun c1 () Int)
(declare-var i6020 Int)
(declare-var i6019 Int)
(declare-var p Int)
(constraint
 (or
  (not
   (and (and (>= i6019 0) (>= i6020 0))
    (and
     (and (and (= i6019 (+ i6020 (mod p 2))) (= i6019 (ite (= p 1) p i6020)))
      (= (ite (= p 1) p i6019) 0))
     (not (= p 1)))))
  (= (+ (+ (mod p 2) i6019) i6020) c1)))
(check-synth)
