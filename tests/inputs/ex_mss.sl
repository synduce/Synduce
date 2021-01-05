(set-logic DTLIA)
(define-fun max ((x Int) (y Int)) Int (ite (> x y) x y))

(synth-fun odot3 ((x181 (Tuple Int Int Int Int)) (x182 (Tuple Int Int Int Int))) Int
 ((Ix Int) (Ic Int))
 ((Ix Int
   (Ic ((_ tupSel 0) x181) ((_ tupSel 1) x181) ((_ tupSel 2) x181) ((_ tupSel 3) x181)
    ((_ tupSel 0) x182) ((_ tupSel 1) x182) ((_ tupSel 2) x182) ((_ tupSel 3) x182)
    (- Ix) (+ Ix Ix) (max Ix Ix)))
  (Ic Int (0 1))))

(declare-var new65 Int)
(declare-var new111 Int)
(declare-var new139 Int)
(declare-var i_151 Int)
(declare-var i_153 Int)
(declare-var i_155 Int)
(declare-var i_157 Int)


(constraint
 (or
  (not
   (and (>= i_153 0)
    (and (>= i_155 0)
     (and (>= i_155 i_151)
      (and (>= i_153 i_151)
       (and (>= i_157 0) (and (>= i_157 i_153) (and (>= i_157 i_151) (>= i_157 i_155)))))))))
  (= i_157 (odot3 (mkTuple 0 0 0 0) (mkTuple i_151 i_153 i_155 i_157)))))

(constraint
 (or
  (not
   (and (>= i_153 0)
    (and (>= i_155 0)
     (and (>= i_155 i_151)
      (and (>= i_153 i_151)
       (and (>= i_157 0) (and (>= i_157 i_153) (and (>= i_157 i_151) (>= i_157 i_155)))))))))
  (= (max i_157 (max (+ i_155 new111) 0))
   (odot3
    (mkTuple (+ 0 new111) (max 0 (+ 0 new111)) (max (+ 0 new111) 0) (max 0 (max (+ 0 new111) 0)))
    (mkTuple i_151 i_153 i_155 i_157)))))

(constraint
 (or
  (not
   (and (>= i_153 0)
    (and (>= i_155 0)
     (and (>= i_155 i_151)
      (and (>= i_153 i_151)
       (and (>= i_157 0) (and (>= i_157 i_153) (and (>= i_157 i_151) (>= i_157 i_155)))))))))
  (= (max (max i_157 (max (+ i_155 new139) 0)) (max (+ (max (+ i_155 new139) 0) new111) 0))
   (odot3
    (mkTuple (+ (+ 0 new139) new111) (max (max 0 (+ 0 new139)) (+ (+ 0 new139) new111))
     (max (+ (max (+ 0 new139) 0) new111) 0)
     (max (max 0 (max (+ 0 new139) 0)) (max (+ (max (+ 0 new139) 0) new111) 0)))
    (mkTuple i_151 i_153 i_155 i_157)))))

(constraint
 (or
  (not
   (and (>= i_153 0)
    (and (>= i_155 0)
     (and (>= i_155 i_151)
      (and (>= i_153 i_151)
       (and (>= i_157 0) (and (>= i_157 i_153) (and (>= i_157 i_151) (>= i_157 i_155)))))))))
  (= (max i_157 (max (+ i_155 new65) 0))
   (odot3 (mkTuple (+ 0 new65) (max 0 (+ 0 new65)) (max (+ 0 new65) 0) (max 0 (max (+ 0 new65) 0)))
    (mkTuple i_151 i_153 i_155 i_157)))))

(check-synth)
