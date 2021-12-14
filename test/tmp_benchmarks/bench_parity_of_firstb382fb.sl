(set-logic NIA)
(declare-var p Int)
(constraint (or (not (not (= (- 1) p))) (= (mod p 2) 1)))
(check-synth)
