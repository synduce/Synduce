(set-logic NIA)
(declare-var i3038 Int)
(declare-var i3037 Int)
(declare-var p Int)
(constraint
 (or (not (and (and (>= i3037 0) (>= i3038 0)) (and (= (ite (= p 1) p i3037) 0) (not (= p 1)))))
  (= (+ (+ (mod p 2) i3037) i3038) 0)))
(check-synth)
