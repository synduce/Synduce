(set-logic DTLIA)
(declare-datatypes ((List 0)) (((nil) (cons (head Int) (tail List)))))
(synth-fun f ((x List)) Int
    ((I Int ) (L List) (B Bool))
    ((I Int (0 1 (head L) (+ I I ) (ite B I I )))
     (L List (nil x (cons I L ) (tail L)))
     (B Bool ((( _ is nil) L) (( _ is cons ) L) (= I I ) ( >= I I )))))

(constraint (= (f (cons 4 nil)) 5))
(constraint (= (f (cons 0 nil)) 1))
(constraint (= (f nil) 0))
(check-synth)