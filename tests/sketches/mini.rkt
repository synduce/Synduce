#lang rosette

(require rosette/lib/synthax)

(define-symbolic a1 a2 a3 integer?)

(define (odot i1 i2)
  (choose (+ - min max)
          (choose (+ - min max)
          (choose i1 i2 (??))
          (choose i1 i2 (??)))
          (choose (+ - min max)
          (choose i1 i2 (??))
          (choose i1 i2 (??)))))

(define obj (time
             (synthesize
              #:forall (list a1 a2 a3)
              #:guarantee (assert (= (max (+ (max (+ 0 a1) 0) a2) 0)
                                     (odot (max a1 0) (max a2 0)))))))

(if (sat? obj)
    (print-forms obj)
    (print "UNSAT"))
