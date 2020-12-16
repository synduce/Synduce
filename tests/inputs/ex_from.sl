(set-logic DTLIA)

(define-fun max ((x Int) (y Int)) Int (ite (> x y) x y))

(define-fun min ((x Int) (y Int)) Int (ite (< x y) x y))

(synth-fun join_0 ((x Int) (x47_0 Int) (x47_1 Int) (x48_0 Int) (x48_1 Int)) Int
    ((I Int) (I1 Int) (I2 Int))
    ((I Int ((Variable Int) (max I1 I1) (+ I1 I1)))
     (I1 Int ((Variable Int) (max I2 I2) (+ I2 I2)))
     (I2 Int ((Variable Int)))
     ))

(synth-fun join_1 ((x Int) (x47_0 Int) (x47_1 Int) (x48_0 Int) (x48_1 Int)) Int
    ((I Int) (I1 Int) (I2 Int))
    ((I Int ((Variable Int) (max I1 I1) (+ I1 I1)))
     (I1 Int ((Variable Int) (max I2 I2) (+ I2 I2)))
     (I2 Int ((Variable Int)))
     ))



(define-fun s0_0 () Int 0)
(define-fun s0_1 () Int 0)

(declare-var a Int)
(declare-var i_ Int)
(declare-var i_45 Int)

(constraint
    (or (< i_45 0)
        (= (+ i_ a) (join_0 a i_ i_45 s0_0 s0_1))))


(constraint
    (or (< i_45 0)
        (= (max (+ i_ a) i_45) (join_1 a i_ i_45 s0_0 s0_1))))


(check-synth)
