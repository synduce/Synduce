(set-logic NIA)
(declare-var p2 Int)
(constraint (or (not (= (mod p2 2) 0)) (= (mod p2 2) 1)))
(check-synth)
