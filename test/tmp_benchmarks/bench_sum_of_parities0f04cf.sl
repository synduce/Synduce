(set-logic NIA)
(synth-fun c1 () Int)
(declare-var i5954 Int)
(declare-var i5953 Int)
(declare-var p Int)
(constraint
 (or
  (not
   (and (and (>= i5953 0) (>= i5954 0))
    (and (and (= i5953 (ite (= p 1) p i5954)) (= (ite (= p 1) p i5953) 0)) (not (= p 1)))))
  (= (+ (+ (mod p 2) i5953) i5954) c1)))
(check-synth)
