open Base
open Lib.Automata


let test_depth = 3

(* Declare symbols. *)

let nil = Terminal.mk "Nil" 0 []

let cons = Terminal.mk "Cons" 2 []

let concat = Terminal.mk "Concat" 2 []

let single = Terminal.mk "Single" 1 []

let node = Terminal.mk "Node" 3 []
let zero = Terminal.mk "0" 0 []
let one = Terminal.mk "1" 0 []
let plus = Terminal.mk "+" 3 []
let times = Terminal.mk "*" 3 []
let tint = Terminal.mk "Int" 1 []
let odot = Terminal.mk "odot" 2 []
let x_0 = Terminal.mk "x_0" 0 []
let f_0 = Terminal.mk "f_0" 1 []

(* Automaton defining a tree grammar *)
let a1 =
  {
    states = [0; 1; 2; 3; 4; 5; 6];
    initial = 0;
    final = [2;3;6];
    sigma = [(10, node);(11,nil);(12,zero);(13,one)];
    transitions = Map.of_alist_exn (module Int)
        [
          0,[10,[1;4;0]; 11,[2]];
          1,[12,[3]];
          4,[10,[5;0;4]; 11,[2]];
          5,[13,[6]];
        ];
  }

let a2 =
  {
    states = [0; 1; 2; 3];
    initial = 0;
    final = [2;3];
    sigma = [(10, concat); (11, single); (12, nil); (13, zero)];
    transitions = Map.of_alist_exn (module Int)
        [
          0,[10,[0;0];11,[1];12,[2]];
          1,[13,[3]]
        ];
  }

let sigma =
  Map.of_alist_exn (module String)
    [
      ("Node", node);
      ("Nil", nil);
      ("Concat", concat);
      ("Single", single);
      ("0", zero);
      ("1", one);
      ("+", plus);
      ("*", plus);
      ("x_0", x_0);
      ("f_0", f_0);
      ("odot", odot)
    ]