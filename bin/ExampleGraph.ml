(* ExampleGraph.ml *)
open Base
open Lib.AGraph

module RInt = Rel0(Int)
module IntGraph = Graph(RInt)
module RSInt = RelSym(Int)
module IntSymGraph = Graph(RSInt)

let ex1 = Alg.((~> [1; 2; 3]) * (~> [4; 5]))
let ex2 = Alg.(((~> [1; 3; 2]) * (~> [4; 5])) + (~@ [1;4]))
let ex2comm = Alg.(((~> [4; 5]) * (~> [1; 3; 2]) + (~@ [1; 4])))
let ex3 = Alg.((~@ [1;4]) + (~@ [3;4]) + (~@ [2;4]) + (~@ [1;5]) + (~@ [3;5]) + (~@ [2;5]))
let ex5 = IntGraph.path [1;2;3;4;5;6] (* A path from 1 to 6, connecting all vertices. *)
;;

Fmt.(
  pf stdout "EX1 : %a@." IntGraph.pp ex1;
  pf stdout "EX2 : %a@." IntGraph.pp ex2;
  pf stdout "EX2: %a@." RSInt.pp (RSInt.fold ex2);
  pf stdout "EX2c: %a@." RSInt.pp (RSInt.fold ex2comm);
  pf stdout "EX3 : %a@." IntGraph.pp ex3;
  pf stdout "EX1 = EX2? %b@." (IntGraph.equal ex2 ex1);
  pf stdout "EX1 = EX3? %b@." (IntGraph.equal ex2 ex3);
  pf stdout "EX2 = EX2 commuted? %b@." (IntGraph.equal ex2 ex2comm);
  pf stdout "(sym) EX2 = EX2 commuted? %b@." (IntSymGraph.equal ex2 ex2comm);
  pf stdout "(path) %a" IntGraph.pp ex5
)
