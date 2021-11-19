(set-logic UFLIA)
(declare-fun f (Int) Int)
(assert
    (forall ((x Int))
        (= x (f (+ x (f x))))))
(check-sat)
