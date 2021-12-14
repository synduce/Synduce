(set-logic NIA)
(declare-var p Int)
(constraint (= (mod p 2) 0))
(check-synth)
