#lang rosette

(require rosette/lib/synthax)

(define-symbolic a b xmps xsum integer?)

(assert (>= xmps 0))

(define (odot i1 i2)
  (cons
   ((choose + - min max)
    ((choose + - min max)
     (choose (car i1) (cdr i1) (car i2) (cdr i2) 0)
     (choose (car i1) (cdr i1) (car i2) (cdr i2) 0))
    ((choose + - min max)
     (choose (car i1) (cdr i1) (car i2) (cdr i2) 0)
     (choose (car i1) (cdr i1) (car i2) (cdr i2) 0))
    )
   ((choose + - min max)
    ((choose + - min max)
     (choose (car i1) (cdr i1) (car i2) (cdr i2) (??))
     (choose (car i1) (cdr i1) (car i2) (cdr i2) (??)))
    ((choose + - min max)
     (choose (car i1) (cdr i1) (car i2) (cdr i2) (??))
     (choose (car i1) (cdr i1) (car i2) (cdr i2) (??))))
   ))

(define (_odot i1 i2)
  (cons
   (max (+ (cdr i1) (car i2)) (car i1))
   (+ (cdr i1) (cdr i2))))

(define (paireq a b)
  (and (= (car a) (car b))
       (= (cdr a) (cdr b))))

(define obj (time
             (synthesize
              #:forall (list a b xmps xsum)
              #:guarantee (assert
                           (and
                            (paireq
                             (odot (cons
                                    (max (+ a 0) 0)
                                    (+ a 0))
                                   (cons xmps xsum))
                             (cons (max (+ a xmps) 0)
                                   (+ xsum a)))
                            (paireq
                             (odot (cons 0 0)
                                   (cons xmps xsum))
                             (cons xmps xsum))
                            (paireq
                             (odot (cons xmps xsum) (cons 0 0))
                             (cons xmps xsum)))
                           ))))


(if (sat? obj)
    (print-forms obj)
    (print "UNSAT"))
