(set-logic ALL)
;; Options --quant-ind [--quant-cf --full-saturate-quant]
(set-option :incremental true)
(set-option :quant-ind true)

(declare-datatype Tree ((nil) (node (a Int) (n Int) (tl Tree) (tr Tree))))

(define-fun-rec sum ((t Tree)) Int
    (match t
        (
            (nil 0)
            ((node a n tl tr) (+ a (sum tl) (sum tr)))
        )
    )
)

(define-fun get_mem ((t Tree)) Int 
    (match t 
        (
            (nil 0)
            ((node a n tl tr) n)
        )
    )
)

(declare-fun memo_is_count (Tree) Bool)
(assert (memo_is_count nil))
(assert (forall 
                ((a Int) (n Int) (tl Tree) (tr Tree)) 
                (=> 
                    (memo_is_count (node a n tl tr)) 
                    (and 
                        (memo_is_count tl)
                        (memo_is_count tr)
                        (= n (+ a (get_mem tl) (get_mem tr)))
                    )
                )
                
        )
)

;; lemma: 
(assert
    (not
        (forall 
                ((t Tree))
                (=>
                    (memo_is_count t)
                    (= (get_mem t) (sum t))
                )
        )
    )
)

(check-sat)
;; ;; Should be unsat, i.e. conjecture holds