(set-option :print-success true)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x
y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x
y))
(push 1)
(pop 1)
(push 1)
(declare-const ex62 Int)
(declare-const i_66 Int)
(declare-const i_68 Int)
(push 1)
(assert (not (or (not (and (>= i_66 0) (>= i_66 i_68))) (and (= i_66 (max 0 (+ (max i_66 (+ 0 i_68)) 0))) (= i_68 (+ (+ 0 i_68) 0))))))
(check-sat)
(pop 1)
(push 1)
(assert (and (and (>= i_66 0) (>= i_66 i_68)) (or (not (= (max (+ i_68 ex62) i_66) (max (max ex62 0) (+ i_68 ex62)))) (not (= (+ i_68 ex62) (+ (+ 0 i_68) ex62))))))
(check-sat)
(pop 1)
(pop 1)
(exit)
