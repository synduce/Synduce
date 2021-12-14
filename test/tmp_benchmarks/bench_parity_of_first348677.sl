(set-logic NIA)
(declare-var p Int)
(constraint (or (not (and (not (= (- 1) p)) (= 0 (mod p 2)))) (= (mod p 2) 0)))
(check-synth)
