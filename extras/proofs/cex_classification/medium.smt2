(set-logic ALL)
(set-option :quant-ind true)

(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))

(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))

(declare-datatypes ((tree 1))
 ((par (T1)
   ((Node (Node_0 T1) (Node_1 (tree T1)) (Node_2 (tree T1)))
    (Leaf (Leaf_0 T1))))))

;; (define-funs-rec ((tree_max ((xtree_max (tree Int))) Int))
;;((match xtree_max
;;   (((Leaf x) x) ((Node a l r) (max a (min (tree_max l) (tree_max r))))))))
(declare-fun tree_max ((tree Int)) Int)
(assert (forall ((x Int)) (= (tree_max (Leaf x)) x)))
(assert
  (forall
    ((x Int) (l (tree Int)) (r (tree Int)))
    (= (tree_max (Node x l r))  (max x (max (tree_max l) (tree_max r))))))

;(define-funs-rec ((tree_min ((xtree_min (tree Int))) Int))
; ((match xtree_min
;   (((Leaf x) x) ((Node a l r) (min a (min (tree_min l) (tree_min r))))))))
(declare-fun tree_min ((tree Int)) Int)
(assert (forall ((x Int)) (= (tree_min (Leaf x)) x)))
(assert
  (forall
    ((x Int) (l (tree Int)) (r (tree Int)))
    (= (tree_min (Node x l r))  (min x (min (tree_min l) (tree_min r))))))

;;(define-funs-rec
;; ((is_bst ((xis_bst (tree Int))) Bool)
;;  (aux ((xaux Int) (xaux0 Int) (xaux1 (tree Int))) Bool))
;; ((aux (tree_max xis_bst) (tree_min xis_bst) xis_bst)
;;  (match xaux1
;;   (((Leaf a) true)
;;    ((Node a l r)
;;     (and (<= xaux0 a)
;;      (and (<= a xaux) (and (aux xaux0 a l) (aux a xaux r)))))))))


(declare-fun is_bst ((tree Int)) Bool)
(declare-fun aux (Int Int (tree Int)) Bool)
(assert (forall ((l (tree Int)))
  (= (is_bst l) (aux (tree_max l) (tree_min l) l))))
(assert (forall ((hi Int) (lo Int) (x Int)) (aux hi lo (Leaf x))))
(assert (forall ((hi Int) (lo Int) (x Int) (l (tree Int)) (r (tree Int)))
    (= (aux hi lo (Node x l r))
     (and (<= lo x)
      (and (<= x hi) (and (aux lo x l) (aux x hi r)))))))

(declare-fun f (Int (tree Int)) Int)
(assert (forall ((xf Int) (a Int)) (= (f xf (Leaf a))  (ite (< a xf) 1 0))))
(assert (forall ((xf Int) (a Int) (l (tree Int)) (r (tree Int)))
  (= (f xf (Node a l r))
   (ite (< a xf) (+ (+ 1 (f xf l)) (f xf r)) (+ (f xf l) (f xf r)))
  )))
(define-fun spec ((xspec Int) (xspec0 (tree Int))) Int (f xspec xspec0))


 (assert
     (exists
      ((xf Int) (a Int) (l (tree Int)) (r (tree Int)))
      (and
         (is_bst (Node a l r))
         (< xf a)
         (>= (tree_min r) xf)
       )
     )
)

;(assert
; (exists ((x Int) (n Int) (l1 (tree Int)) (l2 (tree Int)))
;  (and (is_bst (Node n l1 l2))
;   (and (and (>= (spec x l1) 0) (>= (spec x l2) 0)) (not (< n x)))
;   (= (spec x l1) 0)
;   (= (spec x l2) 1))
;   ))

(check-sat)
