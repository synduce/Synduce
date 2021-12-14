(set-logic NIA)
(declare-var p1 Int)
(constraint (or (not (and (> p1 0) (= (mod p1 2) 0))) (= (ite (= (mod p1 2) 1) p1 0) 1)))
(check-synth)
