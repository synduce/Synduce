(set-logic LIA)

(synth-fun f ((x59 Int)) Int)

(declare-var x Int)

(constraint (= x (f (+ x (f x)))))

(check-synth)
