(set-logic NIA)
(declare-var p Int)
(constraint (= (mod p 2) 1))
(check-synth)
