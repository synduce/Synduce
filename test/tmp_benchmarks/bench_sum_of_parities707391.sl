(set-logic NIA)
(declare-var i0 Int)
(declare-var i Int)
(declare-var p Int)
(constraint (or (not (and (>= i 0) (>= i0 0))) (= (+ (+ (mod p 2) i) i0) 0)))
(check-synth)
