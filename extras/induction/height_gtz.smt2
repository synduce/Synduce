(set-option :print-success true)
(set-logic ALL)
(set-option :quant-ind true)
(set-option :produce-models true)
(set-option :incremental true)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))

(declare-datatype Tree
  (
    (node (label Int) (left Tree) (right Tree))
    (nil)
  ))

(declare-fun balanced (Tree) Bool)
(declare-fun height (Tree) Int)
(declare-fun count (Tree) Int)

(assert (= (balanced nil) true))
(assert
  (forall ((a Int) (b Tree) (c Tree))
      (=
        (balanced (node a b c))
        (and (= (height b) (height c)) (and (balanced b) (balanced c)))
      )))
(assert (= (height nil) 0))
(assert
  (forall ((a Int) (b Tree) (c Tree))
      (=
        (height (node a b c))
        (+ 1 (max (height b) (height c))))))

(assert (= (count nil) 0))
(assert
  (forall ((a Int) (b Tree) (c Tree))
    (=
      (count (node a b c))
      (+ 1 (+ (count b) (count c))))))


(declare-fun lemma_0 (Int Int Int) Bool)
(assert (forall ((p Int) (i1 Int) (i2 Int))
  (= (lemma_0 p i1 i2) (= i1 i2))))


;;(assert
;;  (forall ((p Tree))
;;    (>= (height p) 0)
; ))

(assert
 (not
  (forall ((p Int) (p0 Tree) (p1 Tree) (i2 Int) (i1 Int))
   (or
    (not
     (and (balanced (node p p0 p1))
      (and (= (count p1) i2) (= (count p0) i1))))
    (lemma_0 p i1 i2)))))
(check-sat)
