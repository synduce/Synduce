(set-logic NIA)
(declare-var i Int)
(declare-var p Int)
(constraint (= (ite (= (mod p 2) 1) p i) 0))
(check-synth)
