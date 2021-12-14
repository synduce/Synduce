(set-logic NIA)
(declare-var p1 Int)
(constraint (or (not (= (mod p1 2) 0)) (= (mod p1 2) 0)))
(check-synth)
