(set-logic NIA)
(synth-fun c1 () Int)
(declare-var i66 Int)
(declare-var i65 Int)
(declare-var p Int)
(constraint
 (or (not (and (and (>= i65 0) (>= i66 0)) (not (= 1 p)))) (= (+ (+ (mod p 2) i65) i66) c1)))
(check-synth)
