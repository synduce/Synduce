open Base
open Lib.Automata
open Lib.PMRS
open Symbol
open Lib.Utils
open Parsexp

(* Declare symbols. *)
let nil = mk "Nil" 0
let node = mk "Node" 3
let zero = mk "0" 0

let gen_int () = VInt (Random.int 20)

(* Automaton defining a tree grammar *)
let a1 =
  {
    states = [0; 1; 2];
    initial = 0;
    final = [2, 0; 1, -1];
    sigma = [(0, nil);(1,node);(2,zero)];
    transitions = Map.of_alist_exn (module Int)
        [
          0,[0,[2]; 1,[1;0;0]];
        ];
  }

let sigma =
  Map.of_alist_exn (module String)
    [
      ("Node", Terminal.mk "Node" 3 []);
      ("Nil", Terminal.mk "Nil" 0 []);
      ("0", Terminal.mk "0" 0 []);
      ("+", Terminal.mk "+" 2 [])
    ]

let r : Sexp.t =
  Single.parse_string_exn
    "((Main (t) (F t)) (F () (Nil) 0) (F () (Node a l r) (+ a (F l) (F r))))"

let pmrs1 = parse_rules sigma r

let t1 = time (fun () -> generate ~depth:3 a1)


let test_automaton () =
  Fmt.(pf stdout "Generated %i terms.@." (List.length t1));
  List.iter ~f:(fun x ->
      if recognize a1 x then ()
      else Fmt.(pf stdout "ERROR")) t1;
  List.iter ~f:(fun x ->
      if recognize ~cont_ok:false a1 x then
        Fmt.(pf stdout "TERM: %a@."
               (Tree.print_tree (fun fmt s -> string fmt s.name)) x)
      else ()
    ) t1

let test_pmrs () =
  match pmrs1 with
  | Ok rules -> Fmt.(pf stdout "RULES:@.%a@." pp_pmrs rules)
  | Error sl ->
    Fmt.(pf stdout "%a@." (list (pair ~sep:comma  string Sexp.pp_hum)) sl)

;;
test_automaton ();;
test_pmrs ()