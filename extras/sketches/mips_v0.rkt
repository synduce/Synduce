#lang rosette

(require rosette/lib/synthax)

;; Maximum in-order prefix sum
(define x_0 (list 0 0))
(define (f_0 x) x)
(define (oplus s a)
  (list (+ (car s) a)
        (max (list-ref s 1) (+ (car s) a))))

;; In-order recursion scheme
(define (F x t)
  (match t
    [(list a l r) (F (oplus (F x l) a) r)]
    [(list) (f_0 x)]))

(define (Main t) (F x_0 t))


;; MIPS with hom-scheme
;; (define s_0 (list 0 0))
(define s_0 (list 0 0))
(define (odot a sl sr)
  (let ([m1 (list-ref sl 1)][m2 (list-ref sr 1)]
        [s1 (list-ref sl 0)][s2 (list-ref sr 0)])
    (list (max (choose a s1 s2 m1 m2 0 1)
                          (choose a s1 s2 m1 m2 0 1)
                          (choose a s1 s2 m1 m2 0 1))
          (max
           ((choose + max) (choose a s1 s2 m1 m2 0 1) (choose a s1 s2 m1 m2 0 1))
           ((choose + max) (choose a s1 s2 m1 m2 0 1) (choose a s1 s2 m1 m2 0 1)
                           (choose a s1 s2 m1 m2 0 1) (choose a s1 s2 m1 m2 0 1))
           ((choose + max) (choose a s1 s2 m1 m2 0 1) (choose a s1 s2 m1 m2 0 1)
                           (choose a s1 s2 m1 m2 0 1) (choose a s1 s2 m1 m2 0 1))
           ((choose + max) (choose a s1 s2 m1 m2 0 1) (choose a s1 s2 m1 m2 0 1)
                           (choose a s1 s2 m1 m2 0 1) (choose a s1 s2 m1 m2 0 1))))))

;; BFTH
(define (bfth t)
  (define (F t)
  (match t
    [(list a l r) (odot a (F l) (F r))]
    [(list) s_0]))
  (F t))

(define tree1 (list))
(define (tree2 a) (list a (list) (list)))
(define (tree3 a b) (list a (tree2 b) (list)))
(define (tree4 a b) (list a (list) (tree2 b)))
(define (tree5 a b c) (list a (tree2 c) (tree2 b)))
(define (tree6 a b c d) (list a (tree4 c d) (list)))
(define (tree7 a b c d) (list a (list) (tree4 c d)))
(define (tree8 a b c d) (list a (tree2 b) (tree4 c d)))
(define (tree9 a b c d) (list a (tree4 c d) (tree2 b)))

(define (constraint t)
  (let ([r1 (Main t)][r2 (bfth t)])
    (and (= (car r1) (car r2))
         (= (cadr r1) (cadr r2)) )))

(define-symbolic a1 a2 a3 a4 a5 integer?)

(define soln
  (time
  (synthesize
   #:forall (list a1 a2 a3 a4 a5)
   #:guarantee (assert
                (and (constraint tree1)
                     (constraint (tree2 a1))
                     (constraint (tree3 a1 a2))
                     (constraint (tree4 a1 a2))
                     (constraint (tree5 a1 a2 a3))
                     (constraint (tree6 a1 a2 a3 a4))
                     (constraint (tree7 a1 a2 a3 a4))
                     (constraint (tree8 a1 a2 a3 a4))
                     (constraint (tree9 a1 a2 a3 a4))
                     )))))

(if (sat? soln) (print-forms soln) (print "UNSAT"))
