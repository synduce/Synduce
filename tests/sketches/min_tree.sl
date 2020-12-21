(set-logic DTLIA)
(define-fun max ((x Int) (y Int)) Int (ite (> x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (< x y) x y))
(synth-fun join ((x50 Int) (x51 Int) (x52 Int)) Int
    (
        (I Int)
    )
    (
        (I Int (x50 x51 x52 (min I I)))
    ))
(synth-fun f0 ((x53 Int)) Int)
(declare-var a Int)
(declare-var x Int)
(declare-var i_ Int)
(declare-var i_48 Int)
(constraint (= (min a (min i_ i_48)) (join a i_ i_48)))
(constraint (= x (f0 x)))
(check-synth)
