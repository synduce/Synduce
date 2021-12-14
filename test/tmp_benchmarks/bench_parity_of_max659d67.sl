(set-logic NIA)
(declare-var p10 Int)
(declare-var p6 Int)
(declare-var p Int)
(constraint
 (or (not (and (= (mod p 2) 0) (and (= (mod p6 2) 0) (= (mod p10 2) 0))))
  (= (mod (ite (and (> p p6) (> p p10)) (mod p 2) (ite (> p6 p10) (mod p6 2) (mod p10 2))) 2) 0)))
(check-synth)
