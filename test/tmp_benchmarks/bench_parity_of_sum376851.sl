(set-logic NIA)
(declare-var p4 Int)
(declare-var p Int)
(constraint (or (not (and (= (mod p 2) 0) (= (mod p4 2) 0))) (= (mod (+ p p4) 2) 1)))
(check-synth)
